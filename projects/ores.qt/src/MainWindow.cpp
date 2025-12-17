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

#include <functional>
#include <QDebug>
#include <QTableView>
#include <QTimer>
#include <QApplication>
#include <QScreen>
#include <QMdiSubWindow>
#include <QPainter>
#include <QPixmap>
#include <QAction>
#include <QCloseEvent>
#include <QImage>
#include <QFile>
#include <QFont>
#include <QIcon>
#include "ui_MainWindow.h"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/MyAccountDialog.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"
#include "ores.comms/eventing/connection_events.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(nullptr),
    eventBus_(std::make_shared<eventing::service::event_bus>()),
    clientManager_(new ClientManager(eventBus_, this)),
    systemTrayIcon_(nullptr), trayContextMenu_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "Creating the main window.";
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
    const QColor connectedColor(100, 200, 100); // Green for connected
    const QColor disconnectedColor(200, 100, 100); // Red for disconnected
    const QColor reconnectingColor(230, 180, 80); // Orange/yellow for reconnecting
    connectedIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_connected_20_regular.svg",
        connectedColor);
    disconnectedIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_disconnected_20_regular.svg", disconnectedColor);
    reconnectingIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_disconnected_20_regular.svg", reconnectingColor);

    ui_->ActionConnect->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_connected_20_regular.svg", iconColor));
    ui_->ActionDisconnect->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_plug_disconnected_20_regular.svg", iconColor));
    ui_->CurrenciesAction->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg", iconColor));
    ui_->ActionAbout->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_star_20_regular.svg", iconColor));
    ui_->ActionAccounts->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));
    ui_->ActionMyAccount->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_20_regular.svg", iconColor));

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this,
        &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this,
        &MainWindow::onDisconnectTriggered);
    connect(ui_->ActionMyAccount, &QAction::triggered, this,
        &MainWindow::onMyAccountTriggered);
    connect(ui_->ActionAbout, &QAction::triggered, this,
        &MainWindow::onAboutTriggered);

    // Connect Window menu actions
    connect(ui_->ActionDetachAll, &QAction::triggered, this,
        &MainWindow::onDetachAllTriggered);
    connect(ui_->menuWindow, &QMenu::aboutToShow, this,
        &MainWindow::onWindowMenuAboutToShow);

    // Connect ClientManager signals
    connect(clientManager_, &ClientManager::connected, this, &MainWindow::updateMenuState);
    connect(clientManager_, &ClientManager::disconnected, this, &MainWindow::updateMenuState);
    connect(clientManager_, &ClientManager::disconnected, this, [this]() {
        ui_->statusbar->showMessage("Disconnected from server.", 5000);
    });
    connect(clientManager_, &ClientManager::reconnecting, this, [this]() {
        connectionStatusIconLabel_->setPixmap(reconnectingIcon_.pixmap(16, 16));
        ui_->statusbar->showMessage("Reconnecting to server...");
    });
    connect(clientManager_, &ClientManager::reconnected, this, [this]() {
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16));
        ui_->statusbar->showMessage("Reconnected to server.", 5000);
    });

    // Connect Currencies action to controller
    // Controller is created immediately but will check connection status
    createControllers();
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [this]() {
        if (currencyController_)
            currencyController_->showListWindow();
    });

    // Connect Accounts action to controller (admin only)
    connect(ui_->ActionAccounts, &QAction::triggered, this, [this]() {
        if (accountController_)
            accountController_->showListWindow();
    });

    // Initially disable data-related actions until logged in
    updateMenuState();

    // Initialize system tray
    if (QSystemTrayIcon::isSystemTrayAvailable()) {
        BOOST_LOG_SEV(lg(), debug) << "System tray is available, initializing...";

        systemTrayIcon_ = new QSystemTrayIcon(this);
        systemTrayIcon_->setIcon(QIcon(":/images/modern-icon.png"));
        systemTrayIcon_->setToolTip("ORE Studio");

        // Create context menu for the tray icon
        trayContextMenu_ = new QMenu(this);
        trayContextMenu_->addAction(ui_->ActionConnect);
        trayContextMenu_->addAction(ui_->ActionDisconnect);
        trayContextMenu_->addSeparator();
        auto* showAction = trayContextMenu_->addAction("Show Window");
        connect(showAction, &QAction::triggered, this, [this]() {
            show();
            raise();
            activateWindow();
        });
        auto* quitAction = trayContextMenu_->addAction("Quit");
        connect(quitAction, &QAction::triggered, qApp, &QApplication::quit);

        systemTrayIcon_->setContextMenu(trayContextMenu_);
        systemTrayIcon_->show();

        // Connect tray icon activation to show window
        connect(systemTrayIcon_, &QSystemTrayIcon::activated, this,
            [this](QSystemTrayIcon::ActivationReason reason) {
                if (reason == QSystemTrayIcon::Trigger ||
                    reason == QSystemTrayIcon::DoubleClick) {
                    show();
                    raise();
                    activateWindow();
                }
            });

        // Subscribe to connection events for tray notifications
        eventSubscriptions_.push_back(
            eventBus_->subscribe<comms::eventing::connected_event>(
                [this](const comms::eventing::connected_event& e) {
                    if (systemTrayIcon_) {
                        QString message = QString("Connected to %1:%2")
                            .arg(QString::fromStdString(e.host))
                            .arg(e.port);
                        QMetaObject::invokeMethod(this, [this, message]() {
                            systemTrayIcon_->showMessage(
                                "ORE Studio",
                                message,
                                QSystemTrayIcon::Information,
                                3000);
                        }, Qt::QueuedConnection);
                    }
                }));

        eventSubscriptions_.push_back(
            eventBus_->subscribe<comms::eventing::disconnected_event>(
                [this](const comms::eventing::disconnected_event&) {
                    if (systemTrayIcon_) {
                        QMetaObject::invokeMethod(this, [this]() {
                            systemTrayIcon_->showMessage(
                                "ORE Studio",
                                "Disconnected from server",
                                QSystemTrayIcon::Warning,
                                3000);
                        }, Qt::QueuedConnection);
                    }
                }));

        eventSubscriptions_.push_back(
            eventBus_->subscribe<comms::eventing::reconnecting_event>(
                [this](const comms::eventing::reconnecting_event&) {
                    if (systemTrayIcon_) {
                        QMetaObject::invokeMethod(this, [this]() {
                            systemTrayIcon_->showMessage(
                                "ORE Studio",
                                "Reconnecting to server...",
                                QSystemTrayIcon::Information,
                                2000);
                        }, Qt::QueuedConnection);
                    }
                }));

        eventSubscriptions_.push_back(
            eventBus_->subscribe<comms::eventing::reconnected_event>(
                [this](const comms::eventing::reconnected_event&) {
                    if (systemTrayIcon_) {
                        QMetaObject::invokeMethod(this, [this]() {
                            systemTrayIcon_->showMessage(
                                "ORE Studio",
                                "Reconnected to server",
                                QSystemTrayIcon::Information,
                                3000);
                        }, Qt::QueuedConnection);
                    }
                }));

        BOOST_LOG_SEV(lg(), info) << "System tray initialized with "
                                  << eventSubscriptions_.size() << " event subscriptions";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "System tray is not available on this system";
    }

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

void MainWindow::closeEvent(QCloseEvent* event) {
    BOOST_LOG_SEV(lg(), debug) << "MainWindow close event triggered";

    // Close all detachable windows first
    // Make a copy of the list since closing windows modifies the original list
    QList<QPointer<DetachableMdiSubWindow>> windowsCopy;
    for (auto* window : allDetachableWindows_) {
        if (window)
            windowsCopy.append(window);
    }

    for (auto window : windowsCopy) {
        if (window) {
            window->close();
        }
    }

    event->accept();
}

MainWindow::~MainWindow() {
    BOOST_LOG_SEV(lg(), debug) << "MainWindow destructor called";
    // ClientManager and Controllers are children, so they will be destroyed automatically.
    // However, we want to ensure windows are closed properly.
    // No manual deletion loop needed as ClientManager ensures IO context lives long enough
    // for normal Qt object destruction if we just let it happen.
    // But explicitly disconnecting client is good practice.
    if (clientManager_) {
        clientManager_->disconnect();
    }
}

void MainWindow::onLoginTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Login action triggered";

    LoginDialog dialog(clientManager_, this);
    const int result = dialog.exec();

    if (result == QDialog::Accepted) {
        username_ = dialog.getUsername();

        // Update controllers with new username if needed
        if (currencyController_) {
            currencyController_->setUsername(QString::fromStdString(username_));
        }
        if (accountController_) {
            accountController_->setUsername(QString::fromStdString(username_));
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully connected and authenticated.";
        ui_->statusbar->showMessage("Successfully connected and logged in.");
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Login cancelled by user.";
    }
}

void MainWindow::updateMenuState() {
    const bool isConnected = clientManager_ && clientManager_->isConnected();
    const bool isAdmin = clientManager_ && clientManager_->isAdmin();

    // Enable/disable menu actions based on connection state
    ui_->CurrenciesAction->setEnabled(isConnected);

    // Enable/disable connect and disconnect actions
    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    // System menu is always visible but only enabled for admin users
    ui_->menuSystem->menuAction()->setEnabled(isAdmin);
    ui_->ActionAccounts->setEnabled(isAdmin);

    // My Account menu item is enabled when connected
    ui_->ActionMyAccount->setEnabled(isConnected);

    // Update connection status icon in status bar
    if (isConnected) {
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16));
    } else {
        connectionStatusIconLabel_->setPixmap(disconnectedIcon_.pixmap(16, 16));
    }

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: "
                               << isConnected << ", Admin: " << isAdmin;
}

void MainWindow::createControllers() {
    BOOST_LOG_SEV(lg(), debug) << "Creating entity controllers.";

    // Create currency controller
    // It now takes ClientManager instead of client shared_ptr
    currencyController_ = std::make_unique<CurrencyController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_),
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

    // Create account controller (admin only functionality)
    accountController_ = std::make_unique<AccountController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_),
        allDetachableWindows_, this);

    // Connect account controller signals to status bar
    connect(accountController_.get(), &AccountController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(accountController_.get(), &AccountController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });

    BOOST_LOG_SEV(lg(), debug) << "Entity controllers created.";
}

void MainWindow::onDisconnectTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Disconnect action triggered.";
    performDisconnectCleanup();
}

void MainWindow::performDisconnectCleanup() {
    if (clientManager_) {
        clientManager_->disconnect();
    }
}

void MainWindow::onAboutTriggered() {
    AboutDialog dialog(this);
    dialog.exec();
}

void MainWindow::onMyAccountTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "My Account triggered";
    MyAccountDialog dialog(clientManager_, this);
    dialog.exec();
}

void MainWindow::onDetachAllTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Detach All triggered";

    for (auto* detachableWindow : allDetachableWindows_) {
        if (detachableWindow && !detachableWindow->isDetached()) {
            detachableWindow->detach();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "All windows detached.";
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
