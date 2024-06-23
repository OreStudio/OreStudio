#include <QTimer>
#include <QApplication>
#include <QSplashScreen>
#include "ui_main_window.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QSplashScreen splash;
    splash.setPixmap(QPixmap("splash_screen.png"));
    splash.show();

    QMainWindow mainWindow;
    Ui::MainWindow window;
    window.setupUi(&mainWindow);

    QTimer::singleShot(1000, &splash, SLOT(close()));
    QTimer::singleShot(1000, &mainWindow, SLOT(show()));

    return QApplication::exec();
}
