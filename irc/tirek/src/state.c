#include <Python.h>

#include "macros.h"
#include "state.h"

static PyObject* state_table;

// get returns a PyObject from the state table. If there is no object by that key,
// it will return Py_None instead.
static PyObject*
state_get(PyObject *self, PyObject *args)
{
	UNUSED(self);
	PyObject *res, *key;

	PyArg_ParseTuple(args, "O", &key);

	res = PyDict_GetItem(state_table, key);

	if(res == NULL) {
		return Py_None;
	}

	return res;
}

// set sets a value in the table.
static PyObject*
state_set(PyObject *self, PyObject *args)
{
	UNUSED(self);
	PyObject *key, *value;

	PyArg_ParseTuple(args, "OO", &key, &value);

	PyDict_SetItem(state_table, key, value);

	return Py_None;
}

static PyMethodDef StateMethods[] = {
	{"get", state_get, METH_VARARGS,
		"Return a python object from the global state table. Returns None if there is no value."},
	{"set", state_set, METH_VARARGS,
		"Set a value in the state table."},
	{NULL, NULL, 0, NULL}
};

static PyModuleDef StateModule = {
	PyModuleDef_HEAD_INIT, "state", "The global state of tirek", -1, StateMethods,
	NULL, NULL, NULL, NULL
};

static PyObject*
PyInit_emb(void)
{
	return PyModule_Create(&StateModule);
}

// state_init initializes the state module.
void state_init()
{
	state_table = PyDict_New();
	PyImport_AppendInittab("state", &PyInit_emb);
}
