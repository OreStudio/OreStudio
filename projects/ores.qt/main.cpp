#include "ui_main_window.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QMainWindow mainWindow;
    Ui::MainWindow window;
    window.setupUi(&mainWindow);
    mainWindow.show();
    return QApplication::exec();
}
