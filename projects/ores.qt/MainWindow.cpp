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
#include <QMdiSubWindow>
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QFile>
#include <QFont>
#include "ui_MainWindow.h"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/CurrencyMdiWindow.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(new MdiAreaWithBackground()) {

    BOOST_LOG_SEV(lg(), info) << "Creating the main window.";
    ui_->setupUi(this);

    // Set up MDI area
    ui_->horizontalLayout_3->addWidget(mdiArea_);
    mdiArea_->setBackgroundLogo("ore-studio-background.png");

    // Configure toolbar to show text under icons
    ui_->toolBar->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    // Set smaller font for toolbar text
    QFont toolbarFont = ui_->toolBar->font();
    toolbarFont.setPointSize(7);
    ui_->toolBar->setFont(toolbarFont);

    // Apply recolored icons for dark theme visibility (light gray color)
    const QColor iconColor(220, 220, 220);
    ui_->ActionConnect->setIcon(createRecoloredIcon(
        "ic_fluent_plug_connected_20_filled.svg", iconColor));
    ui_->ActionDisconnect->setIcon(createRecoloredIcon(
        "ic_fluent_plug_disconnected_20_filled.svg", iconColor));
    ui_->CurrenciesAction->setIcon(createRecoloredIcon(
        "ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this, &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this, &MainWindow::onDisconnectTriggered);

    // Currencies action creates MDI window with currency table
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [=, this]() {
        using ores::utility::log::warn;
        using ores::utility::log::info;

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), warn) << "Currencies action triggered but not connected";
            QMessageBox::warning(this, "Not Connected",
                "Please login first to view currencies.");
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Creating currencies MDI window";
        auto* currencyWidget = new CurrencyMdiWindow(client_, this);

        // Connect status signals to status bar
        connect(currencyWidget, &CurrencyMdiWindow::statusChanged,
                this, [this](const QString& message) {
            ui_->statusbar->showMessage(message);
        });
        connect(currencyWidget, &CurrencyMdiWindow::errorOccurred,
                this, [this](const QString& error_message) {
            ui_->statusbar->showMessage("Error loading currencies: " + error_message);
        });

        auto* subWindow = mdiArea_->addSubWindow(currencyWidget);
        subWindow->setWindowTitle("Currencies");
        subWindow->setWindowIcon(createRecoloredIcon(
            "ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));
        subWindow->showMaximized();
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

    // Enable/disable connect and disconnect actions
    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: " << isConnected;
}

void MainWindow::onDisconnectTriggered() {
    BOOST_LOG_SEV(lg(), info) << "Disconnect action triggered";

    if (client_ && client_->is_connected()) {
        client_->disconnect();

        // Reset work guard to allow IO context to finish
        work_guard_.reset();

        // Stop IO context and join thread
        if (io_context_) {
            io_context_->stop();
        }

        if (io_thread_ && io_thread_->joinable()) {
            io_thread_->join();
        }

        // Clear client infrastructure
        client_.reset();
        io_thread_.reset();
        io_context_.reset();

        updateMenuState();

        BOOST_LOG_SEV(lg(), info) << "Disconnected from server";
        QMessageBox::information(this, "Disconnected",
            "Successfully disconnected from the server.");
    }
}

QIcon MainWindow::createRecoloredIcon(const QString& svgPath, const QColor& color) {
    // Qt6 can load SVG files directly into QIcon
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load SVG: " << svgPath.toStdString();
        return QIcon();
    }

    // Create recolored icon at multiple sizes
    QIcon recoloredIcon;
    for (int size : {16, 20, 24, 32, 48, 64}) {
        // Get pixmap from original icon
        QPixmap pixmap = originalIcon.pixmap(size, size);

        // Create a new image for the recolored version
        QImage image = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);

        // Apply color to all pixels while preserving alpha
        for (int y = 0; y < image.height(); ++y) {
            for (int x = 0; x < image.width(); ++x) {
                QColor pixelColor = image.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    image.setPixelColor(x, y, pixelColor);
                }
            }
        }

        recoloredIcon.addPixmap(QPixmap::fromImage(image));
    }

    return recoloredIcon;
}

}
