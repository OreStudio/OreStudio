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
#include <QFileDialog>
#include <QSettings>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QStandardPaths>
#include "ui_MainWindow.h"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/MyAccountDialog.hpp"
#include "ores.qt/SessionHistoryDialog.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/RoleController.hpp"
#include "ores.qt/FeatureFlagController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"
#include "ores.qt/EventViewerDialog.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/TelemetrySettingsDialog.hpp"
#include "ores.comms/eventing/connection_events.hpp"
#include "ores.utility/version/version.hpp"

namespace ores::qt {

using namespace ores::telemetry::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(nullptr),
    eventBus_(std::make_shared<eventing::service::event_bus>()),
    clientManager_(new ClientManager(eventBus_, this)),
    imageCache_(new ImageCache(clientManager_, this)),
    systemTrayIcon_(nullptr), trayContextMenu_(nullptr),
    instanceColorIndicator_(nullptr), eventViewerWindow_(nullptr) {

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
    ui_->ActionRoles->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_lock_closed_20_regular.svg", iconColor));
    ui_->ActionFeatureFlags->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_checkmark_20_regular.svg", iconColor));
    ui_->ActionMyAccount->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_20_regular.svg", iconColor));
    ui_->ActionMySessions->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_clock_16_regular.svg", iconColor));
    ui_->ExitAction->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_dismiss_20_regular.svg", iconColor));
    ui_->ActionOpenRecording->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_open_20_regular.svg", iconColor));
    ui_->ActionSetRecordingDirectory->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor));
    ui_->ActionTelemetrySettings->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_settings_20_regular.svg", iconColor));

    // Create record icons - regular (gray) for off, filled (red) for on
    recordOffIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_record_20_regular.svg", iconColor);
    recordOnIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_record_20_filled.svg", QColor(220, 80, 80)); // Red for recording
    ui_->ActionRecordSession->setIcon(recordOffIcon_);
    ui_->ActionEventViewer->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_document_code_16_regular.svg", iconColor));

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this,
        &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this,
        &MainWindow::onDisconnectTriggered);
    connect(ui_->ActionMyAccount, &QAction::triggered, this,
        &MainWindow::onMyAccountTriggered);
    connect(ui_->ActionMySessions, &QAction::triggered, this,
        &MainWindow::onMySessionsTriggered);
    connect(ui_->ActionAbout, &QAction::triggered, this,
        &MainWindow::onAboutTriggered);
    connect(ui_->ExitAction, &QAction::triggered, this, &QMainWindow::close);

    // Connect Window menu actions
    connect(ui_->ActionDetachAll, &QAction::triggered, this,
        &MainWindow::onDetachAllTriggered);
    connect(ui_->menuWindow, &QMenu::aboutToShow, this,
        &MainWindow::onWindowMenuAboutToShow);

    // Connect Protocol recording actions
    connect(ui_->ActionRecordSession, &QAction::toggled, this,
        &MainWindow::onRecordSessionToggled);
    connect(ui_->ActionOpenRecording, &QAction::triggered, this,
        &MainWindow::onOpenRecordingTriggered);
    connect(ui_->ActionSetRecordingDirectory, &QAction::triggered, this,
        &MainWindow::onSetRecordingDirectoryTriggered);
    connect(ui_->ActionTelemetrySettings, &QAction::triggered, this,
        [this]() {
            TelemetrySettingsDialog dialog(this);
            dialog.exec();
        });

    // Connect Event Viewer action
    connect(ui_->ActionEventViewer, &QAction::triggered, this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Event Viewer action triggered";

        // If window already exists, just activate it
        if (eventViewerWindow_) {
            eventViewerWindow_->showNormal();
            mdiArea_->setActiveSubWindow(eventViewerWindow_);
            return;
        }

        // Create the event viewer widget
        auto* eventViewer = new EventViewerWindow(eventBus_, clientManager_, this);

        // Wrap in MDI sub-window
        eventViewerWindow_ = new DetachableMdiSubWindow();
        eventViewerWindow_->setWidget(eventViewer);
        eventViewerWindow_->setWindowTitle("Event Viewer");
        eventViewerWindow_->setAttribute(Qt::WA_DeleteOnClose);
        eventViewerWindow_->resize(1000, 600);

        // Track window destruction
        connect(eventViewerWindow_, &QObject::destroyed, this, [this]() {
            eventViewerWindow_ = nullptr;
        });

        mdiArea_->addSubWindow(eventViewerWindow_);
        allDetachableWindows_.append(eventViewerWindow_);
        eventViewerWindow_->show();
    });

    // Connect recording signals
    connect(clientManager_, &ClientManager::recordingStarted, this, [this](const QString& filePath) {
        ui_->statusbar->showMessage(QString("Recording to: %1").arg(filePath), 5000);
        // Ensure icon shows recording is active
        ui_->ActionRecordSession->setIcon(recordOnIcon_);
        ui_->ActionRecordSession->blockSignals(true);
        ui_->ActionRecordSession->setChecked(true);
        ui_->ActionRecordSession->blockSignals(false);
    });
    connect(clientManager_, &ClientManager::recordingStopped, this, [this]() {
        ui_->statusbar->showMessage("Recording stopped.", 3000);
        // Ensure checkbox is unchecked when recording stops externally
        // Block signals to prevent recursive toggle
        ui_->ActionRecordSession->blockSignals(true);
        ui_->ActionRecordSession->setChecked(false);
        ui_->ActionRecordSession->setIcon(recordOffIcon_);
        ui_->ActionRecordSession->blockSignals(false);
    });

    // Connect ClientManager signals
    connect(clientManager_, &ClientManager::connected, this, &MainWindow::updateMenuState);
    connect(clientManager_, &ClientManager::disconnected, this, &MainWindow::updateMenuState);
    connect(clientManager_, &ClientManager::disconnected, this, [this]() {
        ui_->statusbar->showMessage("Disconnected from server.", 5000);
        username_.clear();
        updateWindowTitle();
    });
    connect(clientManager_, &ClientManager::reconnecting, this, [this]() {
        connectionStatusIconLabel_->setPixmap(reconnectingIcon_.pixmap(16, 16));
        ui_->statusbar->showMessage("Reconnecting to server...");
    });
    connect(clientManager_, &ClientManager::reconnected, this, [this]() {
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16));
        ui_->statusbar->showMessage("Reconnected to server.", 5000);
    });

    // Load image cache when connected
    connect(clientManager_, &ClientManager::connected, this, [this]() {
        imageCache_->loadAll();
        // Preload all available images for the flag selector to avoid on-demand loading delay
        if (!imageCache_->hasImageList()) {
            imageCache_->loadImageList();
        } else {
            imageCache_->loadAllAvailableImages();
        }
    });

    // When image list is loaded, automatically fetch the actual images
    // This ensures that preloading gets all images ready for the FlagSelectorDialog
    connect(imageCache_, &ImageCache::imageListLoaded,
            imageCache_, &ImageCache::loadAllAvailableImages);

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

    // Connect Roles action to controller (admin only)
    connect(ui_->ActionRoles, &QAction::triggered, this, [this]() {
        if (roleController_)
            roleController_->showListWindow();
    });

    // Connect Feature Flags action to controller (admin only)
    connect(ui_->ActionFeatureFlags, &QAction::triggered, this, [this]() {
        if (featureFlagController_)
            featureFlagController_->showListWindow();
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

    // Set initial window title (version only, no connection info yet)
    updateWindowTitle();

    BOOST_LOG_SEV(lg(), info) << "Main window created.";
}

void MainWindow::closeEvent(QCloseEvent* event) {
    BOOST_LOG_SEV(lg(), debug) << "MainWindow close event triggered";

    // Ask user for confirmation before exiting
    const auto reply = MessageBoxHelper::question(
        this,
        tr("Exit ORE Studio"),
        tr("Are you sure you want to exit?"),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Exit cancelled by user";
        event->ignore();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "User confirmed exit, closing application";

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
        if (roleController_) {
            roleController_->setUsername(QString::fromStdString(username_));
        }
        if (featureFlagController_) {
            featureFlagController_->setUsername(QString::fromStdString(username_));
        }

        // Update window title with username and server info
        updateWindowTitle();

        BOOST_LOG_SEV(lg(), info) << "Successfully connected and authenticated.";
        ui_->statusbar->showMessage("Successfully connected and logged in.");
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Login cancelled by user.";
    }
}

void MainWindow::updateMenuState() {
    const bool isConnected = clientManager_ && clientManager_->isConnected();

    // Enable/disable menu actions based on connection state
    ui_->CurrenciesAction->setEnabled(isConnected);

    // Enable/disable connect and disconnect actions
    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    // System menu enabled when connected - permission checks happen server-side via RBAC
    ui_->menuSystem->menuAction()->setEnabled(isConnected);
    ui_->ActionAccounts->setEnabled(isConnected);
    ui_->ActionRoles->setEnabled(isConnected);
    ui_->ActionFeatureFlags->setEnabled(isConnected);

    // My Account and My Sessions menu items are enabled when connected
    ui_->ActionMyAccount->setEnabled(isConnected);
    ui_->ActionMySessions->setEnabled(isConnected);

    // Protocol recording can be enabled before connection (will start on connect)
    // Only disable when disconnecting if we were recording
    if (!isConnected && clientManager_ && clientManager_->isRecording()) {
        // Keep the checkbox checked but recording is now pending
        ui_->statusbar->showMessage("Recording pending - will start on next connection", 3000);
    }

    // Update connection status icon in status bar
    if (isConnected) {
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16));
    } else {
        connectionStatusIconLabel_->setPixmap(disconnectedIcon_.pixmap(16, 16));
    }

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: " << isConnected;
}

void MainWindow::createControllers() {
    BOOST_LOG_SEV(lg(), debug) << "Creating entity controllers.";

    // Create currency controller
    currencyController_ = std::make_unique<CurrencyController>(
        this, mdiArea_, clientManager_, imageCache_,
        QString::fromStdString(username_), allDetachableWindows_, this);

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

    // Create role controller (admin only functionality)
    roleController_ = std::make_unique<RoleController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_),
        allDetachableWindows_, this);

    // Connect role controller signals to status bar
    connect(roleController_.get(), &RoleController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(roleController_.get(), &RoleController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });

    // Create feature flag controller (admin only functionality)
    featureFlagController_ = std::make_unique<FeatureFlagController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_),
        allDetachableWindows_, this);

    // Connect feature flag controller signals to status bar
    connect(featureFlagController_.get(), &FeatureFlagController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(featureFlagController_.get(), &FeatureFlagController::errorMessage,
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

void MainWindow::onMySessionsTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "My Sessions triggered";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Not connected, cannot show sessions";
        return;
    }

    const auto accountId = clientManager_->accountId();
    if (!accountId) {
        BOOST_LOG_SEV(lg(), warn) << "No account ID available";
        return;
    }

    const QString username = QString::fromStdString(clientManager_->currentUsername());

    auto* sessionDialog = new SessionHistoryDialog(clientManager_, this);
    sessionDialog->setAccount(*accountId, username);
    sessionDialog->setAttribute(Qt::WA_DeleteOnClose);
    sessionDialog->setModal(false);
    sessionDialog->show();
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

void MainWindow::onRecordSessionToggled(bool checked) {
    BOOST_LOG_SEV(lg(), debug) << "Record Session toggled: " << (checked ? "on" : "off");

    if (checked) {
        // Get recording directory from settings or prompt user
        QSettings settings;
        QString recordingDir = settings.value("telemetry/recording_directory").toString();

        if (recordingDir.isEmpty()) {
            // First time - prompt user to select directory
            recordingDir = QFileDialog::getExistingDirectory(
                this,
                tr("Select Recording Directory"),
                QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation),
                QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

            if (recordingDir.isEmpty()) {
                // User cancelled - uncheck the action without triggering slot again
                ui_->ActionRecordSession->blockSignals(true);
                ui_->ActionRecordSession->setChecked(false);
                ui_->ActionRecordSession->blockSignals(false);
                return;
            }

            // Save the directory for future use
            settings.setValue("telemetry/recording_directory", recordingDir);
            BOOST_LOG_SEV(lg(), info) << "Recording directory set to: "
                                      << recordingDir.toStdString();
        }

        // Enable recording (works before or after connection)
        if (clientManager_->enableRecording(recordingDir.toStdString())) {
            // Update icon to show recording is active
            ui_->ActionRecordSession->setIcon(recordOnIcon_);
            if (clientManager_->isConnected()) {
                BOOST_LOG_SEV(lg(), info) << "Recording enabled to: "
                                          << recordingDir.toStdString();
            } else {
                BOOST_LOG_SEV(lg(), info) << "Recording will start on connect to: "
                                          << recordingDir.toStdString();
                ui_->statusbar->showMessage(
                    QString("Recording enabled - will start when connected"), 5000);
            }
        } else {
            BOOST_LOG_SEV(lg(), error) << "Failed to enable recording";
            ui_->statusbar->showMessage("Failed to enable recording", 5000);
            ui_->ActionRecordSession->blockSignals(true);
            ui_->ActionRecordSession->setChecked(false);
            ui_->ActionRecordSession->blockSignals(false);
        }
    } else {
        // Disable recording
        clientManager_->disableRecording();
        // Update icon to show recording is off
        ui_->ActionRecordSession->setIcon(recordOffIcon_);
        BOOST_LOG_SEV(lg(), info) << "Recording disabled";
    }
}

void MainWindow::onOpenRecordingTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Open Recording triggered";

    // Get the last used recording directory as the starting point
    QSettings settings;
    QString startDir = settings.value("telemetry/recording_directory",
        QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation)).toString();

    QString filePath = QFileDialog::getOpenFileName(
        this,
        tr("Open Recording"),
        startDir,
        tr("ORE Studio Recordings (*.ores);;All Files (*)"));

    if (filePath.isEmpty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening recording: " << filePath.toStdString();

    // TODO: Implement recording viewer dialog
    // For now, just show a message
    ui_->statusbar->showMessage(QString("Opening recording: %1 (viewer not yet implemented)")
        .arg(filePath), 5000);
}

void MainWindow::onSetRecordingDirectoryTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Set Recording Directory triggered";

    QSettings settings;
    QString currentDir = settings.value("telemetry/recording_directory",
        QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation)).toString();

    QString newDir = QFileDialog::getExistingDirectory(
        this,
        tr("Select Recording Directory"),
        currentDir,
        QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

    if (newDir.isEmpty()) {
        return; // User cancelled
    }

    settings.setValue("telemetry/recording_directory", newDir);
    BOOST_LOG_SEV(lg(), info) << "Recording directory updated to: " << newDir.toStdString();
    ui_->statusbar->showMessage(QString("Recording directory set to: %1").arg(newDir), 5000);

    // If currently recording, update the directory for the next recording
    if (clientManager_) {
        clientManager_->setRecordingDirectory(newDir.toStdString());
    }
}

void MainWindow::setInstanceInfo(const QString& name, const QColor& color) {
    instanceName_ = name;
    instanceColor_ = color;

    BOOST_LOG_SEV(lg(), info) << "Instance info set: name='" << name.toStdString()
                              << "', color=" << (color.isValid() ? color.name().toStdString() : "none");

    // Create/update the status bar indicator if color is specified
    if (color.isValid()) {
        if (!instanceColorIndicator_) {
            // Create a small colored indicator for the status bar
            instanceColorIndicator_ = new QLabel(this);
            instanceColorIndicator_->setFixedSize(16, 16);
            // Insert before the connection status icon (at position 0 of permanent widgets)
            ui_->statusbar->insertPermanentWidget(0, instanceColorIndicator_);
        }

        // Style as a colored circle with the instance color
        instanceColorIndicator_->setStyleSheet(
            QString("background-color: %1; border-radius: 8px; border: 1px solid rgba(255,255,255,50);")
                .arg(color.name()));
        instanceColorIndicator_->setToolTip(name.isEmpty() ? tr("Instance") : name);
        instanceColorIndicator_->show();
    } else if (instanceColorIndicator_) {
        // Hide the indicator if no color specified
        instanceColorIndicator_->hide();
    }

    updateWindowTitle();
}

void MainWindow::updateWindowTitle() {
    QString title = QString("ORE Studio v%1").arg(ORES_VERSION);

    // Add connection info if connected
    if (clientManager_ && clientManager_->isConnected()) {
        QString serverInfo = QString::fromStdString(clientManager_->serverAddress());
        if (!username_.empty()) {
            title += QString(" - %1@%2")
                .arg(QString::fromStdString(username_))
                .arg(serverInfo);
        } else {
            title += QString(" - %1").arg(serverInfo);
        }
    }

    // Add instance name if set
    if (!instanceName_.isEmpty()) {
        title += QString(" [%1]").arg(instanceName_);
    }

    setWindowTitle(title);
    BOOST_LOG_SEV(lg(), debug) << "Window title updated: " << title.toStdString();
}

}
