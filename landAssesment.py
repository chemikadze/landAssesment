#!/usr/bin/python

import os
import sys

from PyQt4.QtCore import *
from PyQt4.QtGui import *
import clips

import sqlite3
from sqlite3.dbapi2 import OperationalError

from pymongo import Connection

from inspect import getargspec

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

    def __init__(self, dao, parent=None):
        super(MainWindow, self).__init__(parent)
        self.dao = dao

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
        self.resize(600, 300)

        self._loadFromDao()

    def _loadFromDao(self):
        for record in self.dao.findAll():
            self.doAddItem(record)

    def newRecord(self):
        edit = LandEdit(parent=self)
        while edit.exec_():
            record = edit.getRecord()
            if self._itemForName(edit.getRecord().name) is None:
                self.doAddRecord(record)
                return
            else:
                QMessageBox.warning(self, "Wrong name", "Lands must have different names!")

    def deleteRecord(self):
        Ok, Cancel = QMessageBox.Ok, QMessageBox.Cancel
        if QMessageBox.question(self, "Delete record?", "Record will be deleted", Ok | Cancel) == Ok:
            self.doDeleteCurrentRecord()

    def editRecord(self):
        record = self._recordForItem(self._recordView.currentItem())
        edit = LandEdit(record, self)
        row = self._recordView.indexFromItem(self._recordView.currentItem()).row()
        while edit.exec_():
            record = edit.getRecord()
            collision = self._itemForName(edit.getRecord().name)
            if collision is None or self._recordView.indexFromItem(collision).row() == row:
                self.doUpdateRecord(record)
                return
            else:
                QMessageBox.warning(self, "Wrong name", "Lands must have different names!")

    def _recordForItem(self, item):
        return QVariant(item.data(0, Qt.UserRole)).toPyObject()

    def _itemForName(self, name):
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
            item = self._recordView.topLevelItem(i)
            record = self._recordForItem(item)
            item.takeChildren()
            assertion = record.toClipsFact(template)
            assertion.Assert()
        clips.Run()
        for fact in clips.FactList():
            if fact.Relation == 'land-maximum-candidate':
                item = self._itemForName(fact.Slots["name"])
                if item is None:
                    raise RuntimeError("Algorithm error: can not find record")
                record = self._recordForItem(item)
                record.berry_of_choice = fact.Slots["berry-of-choice"]
                record.profit = fact.Slots["profit"]
                self.doUpdateItem(item, record)
                if fact.Slots["checked"] == "FALSE": # max indicator
                    item.setBackgroundColor(item.columnCount()-1, COOL_COLOR)
                else:
                    item.setBackgroundColor(item.columnCount()-1, NORMAL_COLOR)
            elif fact.Relation == 'berry-profit-option':
                item = self._itemForName(fact.Slots["land-name"])
                if item is None:
                    raise RuntimeError("Algorithm error: can not find record")
                newItem = QTreeWidgetItem()
                pos = len(self.recordLabels) - 2
                newItem.setText(pos, "%s (%s)" %
                                     (str(fact.Slots["name"]), str(fact.Slots["method"])))
                newItem.setText(pos + 1, str(fact.Slots["profit"]))
                item.addChild(newItem)
        self._recordView.expandAll()
        for i in xrange(self._recordView.columnCount()):
            self._recordView.resizeColumnToContents(i)

    def doAddRecord(self, record):
        item = self.doAddItem(record)
        self.doUpdateItem(item, record)
        self.dao.createRecord(record)

    def doAddItem(self, record):
        row = self._recordView.topLevelItemCount()
        item = QTreeWidgetItem()
        item.setFlags(item.flags() & ~Qt.ItemIsEditable)
        self._recordView.addTopLevelItem(item)
        self._recordView.setCurrentItem(self._recordView.topLevelItem(row))
        self.doUpdateItem(item, record)
        return item

    def doUpdateRecord(self, record):
        item = self._recordView.currentItem()
        self.doUpdateItem(item, record)
        self.dao.updateRecord(record)

    def doUpdateItem(self, item, record):
        fields = ["name", "square"] + map(lambda x: x[0], CHARACTERISTICS) + ["berry_of_choice", "profit"]
        if item is not None:
            for (i, field) in zip(xrange(len(fields)), fields):
                value = getattr(record, field)
                if not value is None:
                    item.setText(i, str(getattr(record, field)))
                else:
                    item.setText(i, "")
                self._recordView.resizeColumnToContents(i)
            item.setData(0, Qt.UserRole, QVariant(record))

    def doDeleteCurrentRecord(self):
        row = self._recordView.currentIndex().row()
        if 0 <= row < self._recordView.topLevelItemCount():
            item = self._recordView.takeTopLevelItem(row)
            self.dao.deleteRecord(self._recordForItem(item))


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
        d["name"] = str(self._nameEdit.text())
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

    def toClipsFact(self, template):
        fact = template.BuildFact()
        fact.AssignSlotDefaults()
        fact.Slots["name"] = str(self.name)
        fact.Slots["square"] = float(self.square)
        for i in ["water", "soil", "surface", "humus", "underlayment"]:
            fact.Slots[i] = clips.Symbol(str(getattr(self, i)))
        return fact


class RecordDao(object):

    def createRecord(self, record):
        raise NotImplementedError()
    
    def updateRecord(self, record):
        raise NotImplementedError()
    
    def findAll(self):
        raise NotImplementedError()
    
    def deleteRecord(self, record):
        raise NotImplementedError()


class SqliteDao(RecordDao):

    SCHEMA = """
        CREATE TABLE lands(
            name VARCHAR(30),
            square FLOAT,
            water VARCHAR(15),
            soil VARCHAR(15),
            surface VARCHAR(15),
            humus VARCHAR(15),
            underlayment VARCHAR(15)
        )
    """

    FIELDS = ["name", "square", "water", "soil", "surface", "humus", "underlayment"]

    def __init__(self, database):
        self.conneciton = sqlite3.connect(database)
        self.__record_table = "lands"
        try:
            self.conneciton.cursor().execute("SELECT COUNT(*) FROM lands")
        except OperationalError as e:
            if "no such table" in str(e):
                self.__init_database()

    def __init_database(self):
        self.conneciton.cursor().execute(self.SCHEMA)

    def _tupled(self, record):
        return map(lambda x: getattr(record, x), self.FIELDS)

    def updateRecord(self, record):
        cur = self.conneciton.cursor()
        update_request = "UPDATE lands SET %(fields)s WHERE name = ?" % {
            'fields': ", ".join(map(lambda x: x + " = ?", self.FIELDS))
        }
        cur.execute(update_request, self._tupled(record) + [record.name])
        self.conneciton.commit()

    def findAll(self):
        cur = self.conneciton.cursor()
        res = cur.execute("SELECT %s FROM lands" % ", ".join(self.FIELDS))
        data = []
        for item in res:
            data.append(Record(*item))
        return data

    def deleteRecord(self, record):
        cur = self.conneciton.cursor()
        cur.execute("DELETE FROM lands WHERE name = ?", (record.name,))
        self.conneciton.commit()

    def createRecord(self, record):
        cur = self.conneciton.cursor()
        insert_request = "INSERT INTO lands VALUES(%s)" % ", ".join(["?"] * len(self.FIELDS))
        cur.execute(insert_request, self._tupled(record))
        self.conneciton.commit()

class MongoDao(RecordDao):

    FIELDS = ["name", "square", "water", "soil", "surface", "humus", "underlayment"]

    def __init__(self, host, database, port=27017):
        self.host = host
        self.port = port
        self.database = database
        self._connection = Connection(host, int(port))
        self._db = getattr(self._connection, database)
        self._records = self._db["lands"]

    def updateRecord(self, record):
        self._records.update({"_id": record.name}, self._document(record))

    def findAll(self):
        return map(lambda x: Record(**self._filterKeys(x)), self._records.find())

    def deleteRecord(self, record):
        self._records.remove({"_id": record.name})

    def createRecord(self, record):
        doc = self._document(record)
        doc["_id"] = record.name
        self._records.insert(doc)

    def _document(self, record):
        return dict(map(lambda x: (x, getattr(record, x)), self.FIELDS))

    def _filterKeys(self, document):
        return dict(filter(lambda p: p[0] in self.FIELDS, document.iteritems()))


class FakeDao(RecordDao):

    def __init__(self):
        self.__items = set([
            Record("preset item", 10, "dry", "big", "cracked", "thick", "medium"),
            Record("preset item2", 7, "throughout", "big", "cracked", "thick", "medium")])

    def createRecord(self, record):
        self.__items.add(record)

    def updateRecord(self, record):
        for item in self.__items:
            if item.name == record.name:
                self.__items.remove(item)
                self.__items.add(record)

    def findAll(self):
        return self.__items

    def deleteRecord(self, record):
        self.__items.remove(record)

    def deleteByName(self, name):
        for item in self.__items:
            if item.name == name:
                self.__items.remove(name)


class ConnectDialog(QDialog):

    class ConstructorFields(QWidget):

        def __init__(self, backend):
            super(QWidget, self).__init__()

            self.argSources = []

            (args, varargs, kw, defaults) = getargspec(backend)
            if defaults is None: defaults = [None] * len(args)
            if defaults is None: defaults = [None] * len(args)
            defaults = [None] * (len(defaults) - len(args)) + list(defaults)

            la = QGridLayout()

            for (caption, default, idx) in zip(args, defaults, xrange(len(args))):
                lbl = QLabel(caption)
                box = QLineEdit()
                if not default is None:
                    box.setText(str(default))

                la.addWidget(lbl, idx, 0)
                la.addWidget(box, idx, 1)
                self.argSources.append(box)

            self.setLayout(la)

        def getArgs(self):
            return map(lambda x: str(x.text()), self.argSources)

    def __init__(self, backends, parent = None):
        super(QDialog, self).__init__(parent)

        self.box = QComboBox()
        for backend in backends:
            self.box.addItem(backend.__name__, QVariant(backend))

        buttons = QDialogButtonBox(QDialogButtonBox.Open | QDialogButtonBox.Cancel)

        self.con_fields = QLabel()

        la = QVBoxLayout(self)
        la.addWidget(self.box)
        la.addWidget(self.con_fields)
        la.addStretch()
        la.addWidget(buttons)
        self.la = la

        self.setLayout(la)

        self.connect(self.box, SIGNAL("activated(int)"), self._backendChanged)
        self.connect(buttons, SIGNAL("accepted()"), self.accept)
        self.connect(buttons, SIGNAL("rejected()"), self.reject)

        self._currentIndex = 0
        self._setCurrentBackend(self.getBackend())

    def getBackend(self):
        return QVariant(self.box.itemData(self._currentIndex)).toPyObject()

    def _backendChanged(self, itemIndex):
        self._currentIndex = itemIndex
        self._setCurrentBackend(self.getBackend())

    def _setCurrentBackend(self, backend):
        self.la.removeWidget(self.con_fields)
        self.con_fields.hide()
        self.con_fields.deleteLater()

        newCons = self.ConstructorFields(backend)

        self.con_fields = newCons
        self.la.insertWidget(1, newCons)

    def getParameters(self):
        return self.con_fields.getArgs()

    def createBackend(self):
        return self.getBackend()(*self.getParameters())


def main():
    app = QApplication(sys.argv)

    def Mongo(host="localhost", port="27017", database="test"):
        return MongoDao(host, database, int(port))

    def SQLite(database="/tmp/land.db"):
        return SqliteDao(database)

    d = ConnectDialog([Mongo, SQLite])
    if d.exec_():
        w = MainWindow(d.createBackend())
        w.show()
        sys.exit(app.exec_())

if __name__ == "__main__":
    main()
