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
#include <QScreen>
#include "ui_MainWindow.h"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/LoginDialog.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(new MdiAreaWithBackground()) {

    BOOST_LOG_SEV(lg(), info) << "Creating the main window.";
    ui_->setupUi(this);

    // Set up MDI area
    ui_->horizontalLayout_3->addWidget(mdiArea_);
    mdiArea_->setBackgroundLogo("ore-studio-logo-black.png");

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this, &MainWindow::onLoginTriggered);

    // Currencies action will be updated in later increment
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [=, this]() {
        BOOST_LOG_SEV(lg(), debug) << "Currencies action triggered (not yet implemented)";
    });

    // Initially disable data-related actions until logged in
    updateMenuState();

    // Set window size and center on screen
    resize(1400, 900);

    // Center window on screen
    if (auto* screen = QApplication::primaryScreen()) {
        const QRect screenGeometry = screen->geometry();
        const int x = (screenGeometry.width() - width()) / 2;
        const int y = (screenGeometry.height() - height()) / 2;
        move(x, y);
        BOOST_LOG_SEV(lg(), debug) << "Window centered at (" << x << ", " << y << ")";
    }

    // Login dialog removed - will be triggered from menu
    BOOST_LOG_SEV(lg(), info) << "Main window created without forced login.";
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

    BOOST_LOG_SEV(lg(), info) << "MainWindow destroyed, client disconnected.";
}

void MainWindow::onLoginTriggered() {
    BOOST_LOG_SEV(lg(), info) << "Login action triggered";

    LoginDialog dialog(this);
    const int result = dialog.exec();

    if (result == QDialog::Accepted) {
        // Transfer ownership of client infrastructure from dialog
        client_ = dialog.getClient();
        io_context_ = dialog.takeIOContext();
        work_guard_ = dialog.takeWorkGuard();
        io_thread_ = dialog.takeIOThread();

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Successfully connected to server and authenticated.";
            updateMenuState();
            QMessageBox::information(this, "Login Successful",
                "Successfully connected and logged in to the server.");
        } else {
            BOOST_LOG_SEV(lg(), error) << "Client is not properly connected after login.";
            QMessageBox::critical(this, "Connection Error",
                "Failed to establish server connection.");
        }
    } else {
        BOOST_LOG_SEV(lg(), info) << "Login cancelled by user.";
    }
}

void MainWindow::updateMenuState() {
    const bool isConnected = client_ && client_->is_connected();

    // Enable/disable menu actions based on connection state
    ui_->CurrenciesAction->setEnabled(isConnected);

    // Update connect action text
    if (isConnected) {
        ui_->ActionConnect->setText("&Disconnect");
    } else {
        ui_->ActionConnect->setText("&Connect");
    }

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: " << isConnected;
}

}
