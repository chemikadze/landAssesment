#!/usr/bin/python

import os
import sys

from PyQt4.QtCore import *
from PyQt4.QtGui import *
import clips

clips.DebugConfig.WatchAll()
clips.DebugConfig.DribbleOn("debug.log")

CHARACTERISTICS = [
    ("water", ("throughout", "nothroughout", "dry")),
    ("soil", ("big", "nut", "tiles")),
    ("surface", ("granular", "corticial", "cracked")),
    ("humus", ("thick", "mthick", "mthin", "thin")),
    ("underlayment", ("thick", "medium", "thin"))
]

ALGO_FILE = os.path.join(os.path.dirname(__file__), "algorithm.clp")
COOL_COLOR = QColor(0, 0, 255, 127)
NORMAL_COLOR = QColor(255, 255, 255)

class MainWindow(QMainWindow):

    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)
        toolbar = self.addToolBar("Record")
        self._addAction = QAction("Add record", toolbar)
        self.connect(self._addAction, SIGNAL("triggered()"), self.newRecord)
        self._deleteAction = QAction("Delete record", toolbar)
        self.connect(self._deleteAction, SIGNAL("triggered()"), self.deleteRecord)
        self._editAction = QAction("Edit record", toolbar)
        self.connect(self._editAction, SIGNAL("triggered()"), self.editRecord)
        toolbar.addActions([self._addAction, self._editAction, self._deleteAction])
        self._toolbar = toolbar

        toolbar = self.addToolBar("Calculation")
        self._addAction = QAction("Run", toolbar)
        toolbar.addAction(self._addAction)
        self._runToolbar = toolbar

        self.connect(self._addAction, SIGNAL("triggered()"), self.calculate)
        self._recordView = QTreeWidget()
        self.recordLabels = ["name", "square"] + map(lambda x: x[0], CHARACTERISTICS) + ["berry", "profit"]
        self._recordView.setColumnCount(len(self.recordLabels))
        self._recordView.setHeaderLabels(QStringList(self.recordLabels))
        for i in xrange(len(self.recordLabels)):
            self._recordView.resizeColumnToContents(i)
        layout = QGridLayout()
        layout.addWidget(self._recordView, 0, 0)
        self.setCentralWidget(self._recordView)
        self.resize(500, 300)

    def newRecord(self):
        edit = LandEdit(parent=self)
        while edit.exec_():
            record = edit.getRecord()
            if self.itemForName(edit.getRecord().name) is None:
                self.doAddRecord(record)
                return
            else:
                QMessageBox.warning(self, "Wrong name", "Lands must have different names!")

    def deleteRecord(self):
        Ok, Cancel = QMessageBox.Ok, QMessageBox.Cancel
        if QMessageBox.question(self, "Delete record?", "Record will be deleted", Ok | Cancel) == Ok:
            self.doDeleteCurrentRecord()

    def editRecord(self):
        record = QVariant(self._recordView.currentItem().data(0, Qt.UserRole)).toPyObject()
        edit = LandEdit(record, self)
        row = self._recordView.indexFromItem(self._recordView.currentItem()).row()
        while edit.exec_():
            record = edit.getRecord()
            collision = self.itemForName(edit.getRecord().name)
            if collision is None or self._recordView.indexFromItem(collision).row() == row:
                self.doEditCurrentRecord(record)
                return
            else:
                QMessageBox.warning(self, "Wrong name", "Lands must have different names!")

    def itemForName(self, name):
        for i in xrange(self._recordView.topLevelItemCount()):
            item = self._recordView.topLevelItem(i)
            if item.text(0) == name:
                return item

    def calculate(self):
        clips.Clear()
        clips.Load(ALGO_FILE)
        clips.Reset()
        template = clips.FindTemplate("land")
        for i in xrange(self._recordView.topLevelItemCount()):
            record = QVariant(self._recordView.topLevelItem(i).data(0, Qt.UserRole)).toPyObject()
            assertion = record.clipsFact(template)
            assertion.Assert()
        clips.Run()
        for fact in clips.FactList():
            if fact.Relation != 'land-maximum-candidate':
                continue
            item = self.itemForName(fact.Slots["name"])
            if item is None:
                raise RuntimeError("Algorithm error: can not find record")
            record = QVariant(item.data(0, Qt.UserRole)).toPyObject()
            record.berry_of_choice = fact.Slots["berry-of-choice"]
            record.profit = fact.Slots["profit"]
            self.doUpdateItem(item, record)
            if fact.Slots["checked"] == "FALSE": # max indicator
                item.setBackgroundColor(item.columnCount()-1, COOL_COLOR)
            else:
                item.setBackgroundColor(item.columnCount()-1, NORMAL_COLOR)

    def doAddRecord(self, record):
        row = self._recordView.topLevelItemCount()
        item = QTreeWidgetItem()
        item.setFlags(item.flags() & ~Qt.ItemIsEditable)
        self._recordView.addTopLevelItem(item)
        self._recordView.setCurrentItem(self._recordView.topLevelItem(row))
        self.doEditCurrentRecord(record)

    def doEditCurrentRecord(self, record):
        item = self._recordView.currentItem()
        self.doUpdateItem(item, record)

    def doUpdateItem(self, item, record):
        fields = ["name", "square"] + map(lambda x: x[0], CHARACTERISTICS) + ["berry_of_choice", "profit"]
        if item is not None:
            for (i, field) in zip(xrange(len(fields)), fields):
                item.setText(i, str(getattr(record, field)))
                self._recordView.resizeColumnToContents(i)
            item.setData(0, Qt.UserRole, QVariant(record))

    def doDeleteCurrentRecord(self):
        row = self._recordView.currentIndex().row()
        if 0 <= row < self._recordView.topLevelItemCount():
            self._recordView.takeTopLevelItem(row)


class LandEdit(QDialog):

    def __init__(self, record=None, parent=None):
        super(LandEdit, self).__init__(parent)
        layout = QGridLayout()
        self._nameLbl = QLabel("Name")
        self._nameEdit = QLineEdit()
        self._squareLbl = QLabel("Square")
        self._squareEdit = QSpinBox()
        self._squareEdit.setRange(1, 1000)
        self._squareEdit.setSuffix("acres")
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        self.connect(buttons, SIGNAL("accepted()"), self.accept)
        self.connect(buttons, SIGNAL("rejected()"), self.reject)
        self._buttons = buttons
        layout.addWidget(self._nameLbl, 0, 0)
        layout.addWidget(self._nameEdit, 0, 1)
        layout.addWidget(self._squareLbl, 1, 0)
        layout.addWidget(self._squareEdit, 1, 1)
        xpos = 2
        for (type, vals) in CHARACTERISTICS:
            label = QLabel(type)
            combo = QComboBox()
            for lbl in vals:
                combo.addItem(lbl, QVariant(lbl))
            setattr(self, "_%sLbl" % type, label)
            setattr(self, "_%sCombo" % type, combo)
            layout.addWidget(label, xpos, 0)
            layout.addWidget(combo, xpos, 1)
            xpos += 1
        layout.addWidget(buttons, layout.rowCount(), 0, 1, 2)
        self.setLayout(layout)
        if record is not None:
            self.setRecord(record)

    def setRecord(self, record):
        self._nameEdit.setText(record.name)
        self._squareEdit.setValue(record.square)
        for (name, vals) in CHARACTERISTICS:
            combo = getattr(self, "_%sCombo" % name)
            value = getattr(record, name)
            combo.setCurrentIndex(combo.findData(QVariant(value)))

    def fieldToTuple(self, field):
        combo = getattr(self, "_%sCombo" % field[0])
        val = field[1][combo.currentIndex()]
        return field[0], val

    def getRecord(self):
        d = dict(map(self.fieldToTuple, CHARACTERISTICS))
        d["name"] = self._nameEdit.text()
        d["square"] = self._squareEdit.value()
        return Record(**d)


class Record(object):

    def __init__(self, name, square, water, soil, surface, humus, underlayment):
        self.name = name
        self.square = square
        self.water = water
        self.soil = soil
        self.surface = surface
        self.humus = humus
        self.underlayment = underlayment
        self.berry_of_choice = None
        self.profit = None

    def clipsFact(self, template):
        fact = template.BuildFact()
        fact.AssignSlotDefaults()
        fact.Slots["name"] = str(self.name)
        fact.Slots["square"] = float(self.square)
        for i in ["water", "soil", "surface", "humus", "underlayment"]:
            fact.Slots[i] = clips.Symbol(str(getattr(self, i)))
        return fact


def main():
    app = QApplication(sys.argv)

    w = MainWindow()
    w.show()

    sys.exit(app.exec_())

if __name__ == "__main__":
    main()
