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
#include "ores.qt/MainWindow.hpp"

#include <QDebug>
#include <QTableView>
#include <QTimer>
#include <QApplication>
#include <QScreen>
#include <QMdiSubWindow>
#include <QPainter>
#include <QPixmap>
#include <QAction>
#include <QImage>
#include <QFile>
#include <QFont>
#include <QIcon>
#include "ui_MainWindow.h"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating the main window.";
    ui_->setupUi(this);

    mdiArea_ = new MdiAreaWithBackground(this);

    setWindowIcon(QIcon(":/images/modern-icon.png"));

    connectionStatusIconLabel_ = new QLabel(this);
    connectionStatusIconLabel_->setFixedWidth(20);
    connectionStatusIconLabel_->setAlignment(Qt::AlignCenter);
    ui_->statusbar->addPermanentWidget(connectionStatusIconLabel_);

    ui_->horizontalLayout_3->addWidget(mdiArea_);

    if (mdiArea_->viewport()) {
        BOOST_LOG_SEV(lg(), debug) << "MDI area viewport initialized successfully";
    } else {
        BOOST_LOG_SEV(lg(), error) << "MDI area viewport is null!";
    }

    mdiArea_->setBackgroundLogo(":/images/ore-studio-background.png");

    const QColor iconColor(220, 220, 220); // Light gray for dark theme
    connectedIcon_ = IconUtils::IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_connected_20_filled.svg",
        iconColor);
    disconnectedIcon_ = IconUtils::IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_disconnected_20_filled.svg", iconColor);

    ui_->ActionConnect->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_connected_20_filled.svg", iconColor));
    ui_->ActionDisconnect->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_disconnected_20_filled.svg", iconColor));
    ui_->CurrenciesAction->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));
    ui_->ActionAbout->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_star_20_regular.svg", iconColor));

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this,
        &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this,
        &MainWindow::onDisconnectTriggered);
    connect(ui_->ActionAbout, &QAction::triggered, this,
        &MainWindow::onAboutTriggered);

    // Connect Window menu actions
    connect(ui_->ActionDetachAll, &QAction::triggered, this,
        &MainWindow::onDetachAllTriggered);
    connect(ui_->ActionReattachAll, &QAction::triggered, this,
        &MainWindow::onReattachAllTriggered);
    connect(ui_->menuWindow, &QMenu::aboutToShow, this,
        &MainWindow::onWindowMenuAboutToShow);

    // Connect Currencies action to controller
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [this]() {
        if (currencyController_)
            currencyController_->showListWindow();
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
        BOOST_LOG_SEV(lg(), debug) << "Window centered at ("
                                   << x << ", " << y << ")";
    }

    BOOST_LOG_SEV(lg(), info) << "Main window created.";
}

MainWindow::~MainWindow() {
    // Disconnect all detachable windows to prevent destroyed signal handlers
    // from accessing member variables during destruction
    for (auto* window : allDetachableWindows_) {
        if (window)
            disconnect(window, &QObject::destroyed, this, nullptr);
    }

    if (client_)
        client_->disconnect();

    // Reset work guard to allow IO context to finish
    work_guard_.reset();

    // Stop IO context and join thread
    if (io_context_)
        io_context_->stop();

    if (io_thread_ && io_thread_->joinable())
        io_thread_->join();

    BOOST_LOG_SEV(lg(), info) << "MainWindow destroyed, client disconnected.";
}

void MainWindow::onLoginTriggered() {
    BOOST_LOG_SEV(lg(), info) << "Login action triggered";

    LoginDialog dialog(this);
    const int result = dialog.exec();

    if (result == QDialog::Accepted) {
        // Transfer ownership of client infrastructure from dialog
        client_ = dialog.getClient();
        username_ = dialog.getUsername();
        io_context_ = dialog.takeIOContext();
        work_guard_ = dialog.takeWorkGuard();
        io_thread_ = dialog.takeIOThread();

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Successfully connected and authenticated.";

            // Create entity controllers after successful login
            createControllers();

            updateMenuState();
            ui_->statusbar->showMessage("Successfully connected and logged in.");
        } else {
            BOOST_LOG_SEV(lg(), error) << "Client is not properly connected after login.";
            MessageBoxHelper::critical(this, "Connection Error",
                "Failed to establish server connection.");
        }
    } else
        BOOST_LOG_SEV(lg(), info) << "Login cancelled by user.";
}

void MainWindow::updateMenuState() {
    const bool isConnected = client_ && client_->is_connected();

    // Enable/disable menu actions based on connection state
    ui_->CurrenciesAction->setEnabled(isConnected);

    // Enable/disable connect and disconnect actions
    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    // Update connection status icon in status bar
    if (isConnected) {
        // Use 16x16 for status bar
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16));
    } else
        connectionStatusIconLabel_->setPixmap(disconnectedIcon_.pixmap(16, 16));

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: "
                               << isConnected;
}

void MainWindow::createControllers() {
    BOOST_LOG_SEV(lg(), info) << "Creating entity controllers";

    // Create currency controller
    currencyController_ = std::make_unique<CurrencyController>(
        this, mdiArea_, client_, QString::fromStdString(username_),
        allDetachableWindows_, this);

    // Connect controller signals to status bar
    connect(currencyController_.get(), &CurrencyController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(currencyController_.get(), &CurrencyController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });

    BOOST_LOG_SEV(lg(), info) << "Entity controllers created";
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

        // Close all windows managed by controllers
        if (currencyController_)
            currencyController_->closeAllWindows();

        // Reset controllers
        currencyController_.reset();

        // Clear client infrastructure
        client_.reset();
        io_thread_.reset();
        io_context_.reset();

        updateMenuState();

        BOOST_LOG_SEV(lg(), info) << "Disconnected from server";
        ui_->statusbar->showMessage(
            "Successfully disconnected from the server.");
    }
}

void MainWindow::onAboutTriggered() {
    AboutDialog dialog(this);
    dialog.exec();
}

void MainWindow::onDetachAllTriggered() {
    BOOST_LOG_SEV(lg(), info) << "Detach All triggered";

    for (auto* detachableWindow : allDetachableWindows_) {
        if (detachableWindow && !detachableWindow->isDetached()) {
            detachableWindow->detach();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "All windows detached";
}

void MainWindow::onReattachAllTriggered() {
    BOOST_LOG_SEV(lg(), info) << "Reattach All triggered";

    for (auto* detachableWindow : allDetachableWindows_) {
        if (detachableWindow && detachableWindow->isDetached()) {
            detachableWindow->reattach();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "All windows reattached";
}

void MainWindow::onWindowMenuAboutToShow() {
    // Remove any existing window list items (everything after the separator)
    QList<QAction*> actions = ui_->menuWindow->actions();
    bool foundSeparator = false;
    for (auto* action : actions) {
        if (action->isSeparator()) {
            foundSeparator = true;
        } else if (foundSeparator) {
            // Remove and delete dynamically created window list items
            ui_->menuWindow->removeAction(action);
            delete action;
        }
    }

    // Add current window list
    if (allDetachableWindows_.isEmpty()) {
        auto* noWindowsAction = ui_->menuWindow->addAction("No Windows Open");
        noWindowsAction->setEnabled(false);
    } else {
        for (int i = 0; i < allDetachableWindows_.size(); ++i) {
            auto* detachableWindow = allDetachableWindows_[i];
            QString windowTitle = detachableWindow->windowTitle();
            if (windowTitle.isEmpty()) {
                windowTitle = QString("Window %1").arg(i + 1);
            }

            // Add indicator if window is detached
            if (detachableWindow->isDetached()) {
                windowTitle += " (Detached)";
            }

            auto* windowAction = ui_->menuWindow->addAction(windowTitle);
            connect(windowAction, &QAction::triggered, this, [detachableWindow, this]() {
                if (detachableWindow->isDetached()) {
                    // For detached windows, just show and activate
                    detachableWindow->show();
                    detachableWindow->raise();
                    detachableWindow->activateWindow();
                } else {
                    // For attached windows, set as active in MDI area
                    mdiArea_->setActiveSubWindow(detachableWindow);
                    detachableWindow->show();
                    detachableWindow->raise();
                    detachableWindow->activateWindow();
                }
            });

            // Check the active window
            if (detachableWindow == mdiArea_->activeSubWindow()) {
                windowAction->setCheckable(true);
                windowAction->setChecked(true);
            }
        }
    }
}

}
