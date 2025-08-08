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

#include <QDebug>
#include <QTableView>
#include <QtSql/QSqlError>
#include "ui_MainWindow.h"
#include "ores.utility/log/logger.hpp"
#include "ores.qt/MainWindow.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.qt.main_window"));

}

namespace ores::qt {

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mainTab_(new MainTabWidget()) {
    ui_->setupUi(this);
    ui_->horizontalLayout_3->addWidget(mainTab_);

    connect(ui_->CurrenciesAction, &QAction::triggered, mainTab_, [=, this]() {
        mainTab_->openCurrencyTabPage();
    });

    // FIXME: test
    database_ = QSqlDatabase::addDatabase("QPSQL");
    database_.setHostName("localhost");
    database_.setDatabaseName("oresdb");
    database_.setPort(5434);
    database_.setPassword("ahV6aehuij6eingohsiajaiT0");
    database_.setUserName("ores");
    if (database_.open())
    {
        BOOST_LOG_SEV(lg, info) << "Opened connection to database.";
    }  else {
        BOOST_LOG_SEV(lg, error) << "Failed to open connection to database: "
                                 << database_.lastError().text().toStdString();
    }
}

}
