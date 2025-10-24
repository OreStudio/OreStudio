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
#include <QMessageBox>
#include <QTimer>
#include <QApplication>
#include "ui_MainWindow.h"
#include "ores.utility/log/logger.hpp"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/LoginDialog.hpp"

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

    // Show login dialog and establish client connection
    login_dialog dialog(this);
    const int result = dialog.exec();

    if (result == QDialog::Accepted) {
        // Transfer ownership of client infrastructure from dialog
        client_ = dialog.get_client();
        io_context_ = dialog.take_io_context();
        work_guard_ = dialog.take_work_guard();
        io_thread_ = dialog.take_io_thread();

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg, info) << "Successfully connected to server and authenticated.";
            // Pass client to main tab widget for use by tab pages
            mainTab_->set_client(client_);
        } else {
            BOOST_LOG_SEV(lg, error) << "Client is not properly connected after login.";
            QMessageBox::critical(this, "Connection Error",
                "Failed to establish server connection. The application may not function correctly.");
        }
    } else {
        // User cancelled login - exit application
        BOOST_LOG_SEV(lg, info) << "Login cancelled by user.";
        QMessageBox::information(this, "Login Cancelled",
            "Login is required to use ORE Studio. The application will now exit.");
        QTimer::singleShot(0, qApp, &QApplication::quit);
        return;
    }

}

MainWindow::~MainWindow() {
    // Disconnect client
    if (client_) {
        client_->disconnect();
    }

    // Reset work guard to allow IO context to finish
    work_guard_.reset();

    // Stop IO context and join thread
    if (io_context_) {
        io_context_->stop();
    }

    if (io_thread_ && io_thread_->joinable()) {
        io_thread_->join();
    }

    BOOST_LOG_SEV(lg, info) << "MainWindow destroyed, client disconnected.";
}

}
