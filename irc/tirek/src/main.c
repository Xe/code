#include <Python.h>

#include "noddy.h"
#include "state.h"

int
main(int argc, char *argv[])
{
	state_init();
	noddy_init();

	Py_Initialize();

	FILE* file = fopen("./state_test.py", "r");
	PyRun_SimpleFile(file, "./state_test.py");
	fclose(file);

	Py_Finalize();

	return 0;
}
