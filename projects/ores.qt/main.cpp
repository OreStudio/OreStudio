/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include <QTimer>
#include <QTableView>
#include <QApplication>
#include <QSplashScreen>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"
#include "ui_main_window.h"
#include "ores.qt/CurrencyModel.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("main"));

}

int main(int argc, char *argv[])
{
    ores::utility::log::logging_configuration cfg;
    cfg.filename("ores.console.log");
    cfg.output_to_console(false);
    cfg.output_directory("log");
    cfg.severity("debug");

    ores::utility::log::scoped_lifecycle_manager slm;
    slm.initialise(cfg);
    BOOST_LOG_SEV(lg, info) << "Started UI.";

    QApplication app(argc, argv);

    QSplashScreen splash;
    splash.setPixmap(QPixmap("splash_screen.png"));
    splash.show();

    QMainWindow mainWindow;
    Ui::MainWindow window;
    window.setupUi(&mainWindow);
    ores::qt::CurrencyModel currencyModel;
    window.currencyTableView->setModel(&currencyModel);

    QTimer::singleShot(1000, &splash, SLOT(close()));
    QTimer::singleShot(1000, &mainWindow, SLOT(show()));

    return QApplication::exec();
}
