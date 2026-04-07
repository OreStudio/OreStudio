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

#include <chrono>
#include <functional>
#include <QDebug>
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
#include <QMessageBox>
#include <QSettings>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QStandardPaths>
#include "ui_MainWindow.h"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/SystemProvisionerWizard.hpp"
#include "ores.qt/TenantProvisioningWizard.hpp"
#include "ores.qt/PartyProvisioningWizard.hpp"
#include "ores.qt/SignUpDialog.hpp"
#include "ores.qt/MyAccountDialog.hpp"
#include "ores.qt/SessionHistoryDialog.hpp"
#include "ores.qt/AdminPlugin.hpp"
#include "ores.qt/ComputePlugin.hpp"
#include "ores.qt/RefdataPlugin.hpp"
#include "ores.qt/LegacyPlugin.hpp"
#include "ores.qt/plugin_context.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"
#include "ores.qt/EventViewerDialog.hpp"
#include "ores.qt/TelemetryMdiWindow.hpp"
#include "ores.qt/ShellMdiWindow.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/TelemetrySettingsDialog.hpp"
#include "ores.qt/ConnectionBrowserMdiWindow.hpp"
#include "ores.qt/MasterPasswordDialog.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(nullptr),
    eventBus_(std::make_shared<eventing::service::event_bus>()),
    clientManager_(new ClientManager(eventBus_, this)),
    imageCache_(new ImageCache(clientManager_, this)),
    changeReasonCache_(new ChangeReasonCache(clientManager_, this)),
    badgeCache_(new BadgeCache(clientManager_, this)),
    systemTrayIcon_(nullptr), trayContextMenu_(nullptr),
    instanceColorIndicator_(nullptr), eventViewerWindow_(nullptr),
    telemetryViewerWindow_(nullptr),
    userStatusWidget_(nullptr), userStatusNameLabel_(nullptr),
    serverStatusWidget_(nullptr), serverStatusNameLabel_(nullptr),
    tenantStatusWidget_(nullptr), tenantStatusNameLabel_(nullptr),
    partyStatusWidget_(nullptr), partyStatusNameLabel_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "Creating the main window.";
    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    mdiArea_ = new MdiAreaWithBackground(this);

    setWindowIcon(QIcon(":/images/modern-icon.png"));

    // Helper: build a status bar chip with a small icon and a name label.
    // The outer widget carries the border/background; the tooltip is set at
    // update time so it always reflects the current value.
    auto makeStatusChip = [this](Icon icon) -> std::pair<QWidget*, QLabel*> {
        auto* chip = new QWidget(this);
        auto* layout = new QHBoxLayout(chip);
        layout->setContentsMargins(6, 0, 8, 0);
        layout->setSpacing(4);

        auto* iconLbl = new QLabel(chip);
        iconLbl->setPixmap(
            IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor)
                .pixmap(14, 14));
        iconLbl->setFixedSize(14, 14);

        auto* nameLbl = new QLabel(chip);
        layout->addWidget(iconLbl);
        layout->addWidget(nameLbl);
        return {chip, nameLbl};
    };

    const QString normalChipStyle =
        "QWidget { border-left: 1px solid palette(mid);"
        " background: palette(alternateBase); }";

    auto [uWidget, uName] = makeStatusChip(Icon::PersonAccounts);
    auto [sWidget, sName] = makeStatusChip(Icon::Server);
    auto [tWidget, tName] = makeStatusChip(Icon::BuildingSkyscraper);
    auto [pWidget, pName] = makeStatusChip(Icon::Organization);

    userStatusWidget_       = uWidget;  userStatusNameLabel_   = uName;
    serverStatusWidget_     = sWidget;  serverStatusNameLabel_ = sName;
    tenantStatusWidget_     = tWidget;  tenantStatusNameLabel_ = tName;
    partyStatusWidget_      = pWidget;  partyStatusNameLabel_  = pName;

    for (auto* w : {uWidget, sWidget, tWidget, pWidget}) {
        w->setStyleSheet(normalChipStyle);
        w->setVisible(false);
    }

    ui_->statusbar->addPermanentWidget(userStatusWidget_);
    ui_->statusbar->addPermanentWidget(serverStatusWidget_);
    ui_->statusbar->addPermanentWidget(tenantStatusWidget_);
    ui_->statusbar->addPermanentWidget(partyStatusWidget_);

    connectionStatusIconLabel_ = new QLabel(this);
    connectionStatusIconLabel_->setFixedWidth(20);
    connectionStatusIconLabel_->setAlignment(Qt::AlignCenter);
    connectionStatusIconLabel_->installEventFilter(this);
    ui_->statusbar->addPermanentWidget(connectionStatusIconLabel_);

    ui_->horizontalLayout_3->addWidget(mdiArea_);

    if (mdiArea_->viewport()) {
        BOOST_LOG_SEV(lg(), debug) << "MDI area viewport initialized successfully";
    } else {
        BOOST_LOG_SEV(lg(), error) << "MDI area viewport is null!";
    }

    mdiArea_->setBackgroundLogo(":/images/ore-studio-background.png");

    connectedIcon_ = IconUtils::createRecoloredIcon(Icon::PlugConnected, IconUtils::ConnectedColor);
    disconnectedIcon_ = IconUtils::createRecoloredIcon(Icon::PlugDisconnected, IconUtils::DisconnectedColor);
    reconnectingIcon_ = IconUtils::createRecoloredIcon(Icon::PlugDisconnected, IconUtils::ReconnectingColor);

    ui_->ActionConnect->setIcon(IconUtils::createRecoloredIcon(Icon::PlugConnected, IconUtils::DefaultIconColor));
    ui_->ActionDisconnect->setIcon(IconUtils::createRecoloredIcon(Icon::PlugDisconnected, IconUtils::DefaultIconColor));
    ui_->ActionAbout->setIcon(IconUtils::createRecoloredIcon(Icon::Star, IconUtils::DefaultIconColor));
    ui_->ActionMyAccount->setIcon(IconUtils::createRecoloredIcon(Icon::Person, IconUtils::DefaultIconColor));
    ui_->ActionMySessions->setIcon(IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));
    ui_->ExitAction->setIcon(IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    ui_->ActionOpenRecording->setIcon(IconUtils::createRecoloredIcon(Icon::FolderOpen, IconUtils::DefaultIconColor));
    ui_->ActionSetRecordingDirectory->setIcon(IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));
    ui_->ActionTelemetrySettings->setIcon(IconUtils::createRecoloredIcon(Icon::Settings, IconUtils::DefaultIconColor));
    ui_->ActionConnectionBrowser->setIcon(IconUtils::createRecoloredIcon(Icon::ServerLink, IconUtils::DefaultIconColor));

    // Create record icons - regular (gray) for off, filled (red) for on
    recordOffIcon_ = IconUtils::createRecoloredIcon(Icon::Record, IconUtils::DefaultIconColor);
    recordOnIcon_ = IconUtils::createRecoloredIcon(Icon::RecordFilled, IconUtils::RecordingOnColor);
    ui_->ActionRecordSession->setIcon(recordOffIcon_);
    ui_->ActionEventViewer->setIcon(IconUtils::createRecoloredIcon(Icon::DocumentCode, IconUtils::DefaultIconColor));
    ui_->ActionTelemetryViewer->setIcon(IconUtils::createRecoloredIcon(Icon::DocumentTable, IconUtils::DefaultIconColor));
    ui_->ActionShell->setIcon(IconUtils::createRecoloredIcon(Icon::Terminal, IconUtils::DefaultIconColor));

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
    connect(ui_->ActionConnectionBrowser, &QAction::triggered, this,
        &MainWindow::onConnectionBrowserTriggered);

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
            allDetachableWindows_.removeOne(eventViewerWindow_);
            eventViewerWindow_ = nullptr;
        });

        mdiArea_->addSubWindow(eventViewerWindow_);
        allDetachableWindows_.append(eventViewerWindow_);
        eventViewerWindow_->show();
    });

    // Connect Telemetry Viewer action
    connect(ui_->ActionTelemetryViewer, &QAction::triggered, this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Telemetry Viewer action triggered";

        // If window already exists, just activate it
        if (telemetryViewerWindow_) {
            telemetryViewerWindow_->showNormal();
            mdiArea_->setActiveSubWindow(telemetryViewerWindow_);
            return;
        }

        // Create the telemetry viewer widget
        auto* telemetryViewer = new TelemetryMdiWindow(
            clientManager_, QString::fromStdString(username_), this);

        // Wrap in MDI sub-window
        telemetryViewerWindow_ = new DetachableMdiSubWindow();
        telemetryViewerWindow_->setWidget(telemetryViewer);
        telemetryViewerWindow_->setWindowTitle("Telemetry Log Viewer");
        telemetryViewerWindow_->setAttribute(Qt::WA_DeleteOnClose);
        telemetryViewerWindow_->resize(1200, 700);

        telemetryViewerWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
            Icon::DocumentTable, IconUtils::DefaultIconColor));

        // Track window destruction
        connect(telemetryViewerWindow_, &QObject::destroyed, this, [this]() {
            allDetachableWindows_.removeOne(telemetryViewerWindow_);
            telemetryViewerWindow_ = nullptr;
        });

        // Connect status messages to status bar
        connect(telemetryViewer, &TelemetryMdiWindow::statusChanged, this,
            [this](const QString& message) {
                ui_->statusbar->showMessage(message, 5000);
            });
        connect(telemetryViewer, &TelemetryMdiWindow::errorOccurred, this,
            [this](const QString& message) {
                ui_->statusbar->showMessage(message, 5000);
            });

        mdiArea_->addSubWindow(telemetryViewerWindow_);
        allDetachableWindows_.append(telemetryViewerWindow_);
        telemetryViewerWindow_->show();
    });

    // Connect Shell action
    connect(ui_->ActionShell, &QAction::triggered, this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Shell action triggered";

        // If window already exists, just activate it
        if (shellWindow_) {
            shellWindow_->showNormal();
            mdiArea_->setActiveSubWindow(shellWindow_);
            return;
        }

        // Create the shell widget
        auto* shellWidget = new ShellMdiWindow(clientManager_, this);

        // Wrap in MDI sub-window
        shellWindow_ = new DetachableMdiSubWindow();
        shellWindow_->setWidget(shellWidget);
        shellWindow_->setWindowTitle("Shell");
        shellWindow_->setAttribute(Qt::WA_DeleteOnClose);
        shellWindow_->resize(800, 500);

        shellWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
            Icon::Terminal, IconUtils::DefaultIconColor));

        // Track window destruction
        connect(shellWindow_, &QObject::destroyed, this, [this]() {
            allDetachableWindows_.removeOne(shellWindow_);
            shellWindow_ = nullptr;
        });

        // Connect status messages to status bar
        connect(shellWidget, &ShellMdiWindow::statusChanged, this,
            [this](const QString& message) {
                ui_->statusbar->showMessage(message, 5000);
            });

        mdiArea_->addSubWindow(shellWindow_);
        allDetachableWindows_.append(shellWindow_);
        shellWindow_->show();
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
        party_name_.clear();
        activeConnectionName_.clear();
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
    connect(clientManager_, &ClientManager::sessionExpired, this, [this]() {
        QMessageBox::warning(this, "Session Expired",
            "Your session has expired after the maximum allowed duration.\n"
            "Please log in again to continue.");
        clientManager_->disconnect();
        showLoginDialog();
    });

    // Load caches when logged in (not just connected - bootstrap mode doesn't auth)
    connect(clientManager_, &ClientManager::loggedIn, this, [this]() {
        imageCache_->loadAll();
        // Preload all available images for the flag selector to avoid on-demand loading delay
        if (!imageCache_->hasImageList()) {
            imageCache_->loadImageList();
        } else {
            imageCache_->loadAllAvailableImages();
        }

        // Load change reasons for entity dialogs
        changeReasonCache_->loadAll();

        // Load badge definitions and mappings for badge rendering
        badgeCache_->loadAll();
    });

    // When image list is loaded, automatically fetch the actual images
    // This ensures that preloading gets all images ready for the FlagSelectorDialog
    connect(imageCache_, &ImageCache::imageListLoaded,
            imageCache_, &ImageCache::loadAllAvailableImages);

    // Show error message when image loading fails (e.g., CRC errors)
    connect(imageCache_, &ImageCache::loadError, this, [this](const QString& message) {
        QMessageBox::warning(this, tr("Image Loading Error"), message);
    });

    // Create admin plugin — controllers are instantiated in on_login()
    adminPlugin_ = std::make_unique<AdminPlugin>(this);
    connect(adminPlugin_.get(), &AdminPlugin::status_message,
            this, [this](const QString& msg) { ui_->statusbar->showMessage(msg); });
    connect(adminPlugin_.get(), &AdminPlugin::window_created,
            this, &MainWindow::onDetachableWindowCreated);
    connect(adminPlugin_.get(), &AdminPlugin::window_destroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create compute plugin — controllers are instantiated in on_login()
    computePlugin_ = std::make_unique<ComputePlugin>(this);
    connect(computePlugin_.get(), &ComputePlugin::status_message,
            this, [this](const QString& msg) { ui_->statusbar->showMessage(msg); });
    connect(computePlugin_.get(), &ComputePlugin::window_created,
            this, &MainWindow::onDetachableWindowCreated);
    connect(computePlugin_.get(), &ComputePlugin::window_destroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create refdata plugin — controllers are instantiated in on_login()
    refdataPlugin_ = std::make_unique<RefdataPlugin>(this);
    connect(refdataPlugin_.get(), &RefdataPlugin::status_message,
            this, [this](const QString& msg) { ui_->statusbar->showMessage(msg); });
    connect(refdataPlugin_.get(), &RefdataPlugin::window_created,
            this, &MainWindow::onDetachableWindowCreated);
    connect(refdataPlugin_.get(), &RefdataPlugin::window_destroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create legacy plugin — controllers are instantiated in on_login()
    legacyPlugin_ = std::make_unique<LegacyPlugin>(this);
    connect(legacyPlugin_.get(), &LegacyPlugin::status_message,
            this, [this](const QString& msg) { ui_->statusbar->showMessage(msg); });
    connect(legacyPlugin_.get(), &LegacyPlugin::window_created,
            this, &MainWindow::onDetachableWindowCreated);
    connect(legacyPlugin_.get(), &LegacyPlugin::window_destroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // (Domain action connections wired by plugin create_menus() after login)

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

        // Connect to ClientManager signals for tray notifications
        connect(clientManager_, &ClientManager::connected, this, [this]() {
            if (systemTrayIcon_) {
                QString message = QString("Connected to %1")
                    .arg(QString::fromStdString(clientManager_->serverAddress()));
                systemTrayIcon_->showMessage("ORE Studio", message,
                    QSystemTrayIcon::Information, 3000);
            }
        });

        connect(clientManager_, &ClientManager::disconnected, this, [this]() {
            if (systemTrayIcon_) {
                systemTrayIcon_->showMessage("ORE Studio",
                    "Disconnected from server",
                    QSystemTrayIcon::Warning, 3000);
            }
        });

        connect(clientManager_, &ClientManager::reconnecting, this, [this]() {
            if (systemTrayIcon_) {
                systemTrayIcon_->showMessage("ORE Studio",
                    "Reconnecting to server...",
                    QSystemTrayIcon::Information, 2000);
            }
        });

        connect(clientManager_, &ClientManager::reconnected, this, [this]() {
            if (systemTrayIcon_) {
                systemTrayIcon_->showMessage("ORE Studio",
                    "Reconnected to server",
                    QSystemTrayIcon::Information, 3000);
            }
        });

        BOOST_LOG_SEV(lg(), info) << "System tray initialized";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "System tray is not available on this system";
    }

    // Restore saved geometry, or fall back to 1400x900 centred on screen
    QSettings settings;
    if (settings.contains("mainwindow/geometry")) {
        restoreGeometry(settings.value("mainwindow/geometry").toByteArray());
        BOOST_LOG_SEV(lg(), debug) << "Restored saved window geometry";
    } else {
        resize(1400, 900);
        if (auto* screen = QApplication::primaryScreen()) {
            const QRect screenGeometry = screen->geometry();
            move((screenGeometry.width() - width()) / 2,
                 (screenGeometry.height() - height()) / 2);
            BOOST_LOG_SEV(lg(), debug) << "No saved geometry; centred window on screen";
        }
    }

    // Set initial window title (version only, no connection info yet)
    updateWindowTitle();

    BOOST_LOG_SEV(lg(), info) << "Main window created.";
}

QString MainWindow::buildConnectionTooltip() const {
    if (!clientManager_) return QString();

    const bool connected = clientManager_->isConnected();
    const auto disc_since = clientManager_->disconnectedSince();
    const auto host = clientManager_->connectedHost();

    // Never connected
    if (host.empty() && !connected && !disc_since) return QString();

    if (connected) {
        const auto sent = clientManager_->bytesSent();
        const auto recv = clientManager_->bytesReceived();
        const auto rtt  = clientManager_->lastRttMs();

        auto format_bytes = [](std::uint64_t b) -> QString {
            if (b < 1024)
                return QString("%1 B").arg(b);
            if (b < 1024 * 1024)
                return QString("%1 KB").arg(b / 1024.0, 0, 'f', 1);
            return QString("%1 MB").arg(b / (1024.0 * 1024.0), 0, 'f', 2);
        };

        return QString("Server: %1:%2\nSent: %3\nReceived: %4\nLatency: %5 ms")
            .arg(QString::fromStdString(host))
            .arg(clientManager_->connectedPort())
            .arg(format_bytes(sent))
            .arg(format_bytes(recv))
            .arg(rtt);
    }

    if (disc_since) {
        const auto secs = std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::steady_clock::now() - *disc_since).count();
        const auto mins = secs / 60;
        QString duration = mins > 0
            ? QString("%1m %2s").arg(mins).arg(secs % 60)
            : QString("%1s").arg(secs);
        return QString("Disconnected for: %1\nRetrying...").arg(duration);
    }

    return QString();
}

bool MainWindow::eventFilter(QObject* watched, QEvent* event) {
    if (watched == connectionStatusIconLabel_ && event->type() == QEvent::ToolTip) {
        connectionStatusIconLabel_->setToolTip(buildConnectionTooltip());
    }
    return QMainWindow::eventFilter(watched, event);
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

    QSettings settings;
    settings.setValue("mainwindow/geometry", saveGeometry());

    // Close all detachable windows first
    // Make a copy of the list since closing windows modifies the original list
    QList<QPointer<DetachableMdiSubWindow>> windowsCopy;
    for (const auto& window : allDetachableWindows_) {
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
    // Delegate to the modeless login dialog implementation
    onModernLoginTriggered();
}

void MainWindow::updateMenuState() {
    const bool isConnected = clientManager_ && clientManager_->isConnected();
    const bool isLoggedIn = clientManager_ && clientManager_->isLoggedIn();

    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    // Plugin-contributed domain menus enabled when logged in
    for (auto* menu : plugin_menus_)
        menu->setEnabled(isLoggedIn);

    // System menu enabled when logged in
    ui_->menuSystem->menuAction()->setEnabled(isLoggedIn);
    // File menu items requiring authentication
    ui_->ActionMyAccount->setEnabled(isLoggedIn);
    ui_->ActionMySessions->setEnabled(isLoggedIn);

    // Telemetry items requiring authentication
    ui_->ActionTelemetryViewer->setEnabled(isLoggedIn);
    ui_->ActionShell->setEnabled(isLoggedIn);

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

    BOOST_LOG_SEV(lg(), debug) << "Menu state updated. Connected: " << isConnected
                              << ", LoggedIn: " << isLoggedIn;
}

void MainWindow::onDisconnectTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Disconnect action triggered.";
    performDisconnectCleanup();
}

void MainWindow::performDisconnectCleanup() {
    if (legacyPlugin_)
        legacyPlugin_->on_logout();
    if (refdataPlugin_)
        refdataPlugin_->on_logout();
    if (computePlugin_)
        computePlugin_->on_logout();
    if (adminPlugin_)
        adminPlugin_->on_logout();

    for (auto* menu : plugin_menus_) {
        menuBar()->removeAction(menu->menuAction());
        delete menu;
    }
    plugin_menus_.clear();

    if (clientManager_)
        clientManager_->disconnect();
}

bool MainWindow::initializeConnectionManager() {
    // Already initialized
    if (connectionManager_) {
        return true;
    }

    QString dataPath = QStandardPaths::writableLocation(
        QStandardPaths::AppDataLocation);
    QDir().mkpath(dataPath);
    QString dbPath = dataPath + "/connections.db";

    BOOST_LOG_SEV(lg(), debug) << "Connections database path: "
                               << dbPath.toStdString();

    try {
        // First, try with stored master password (or empty if none)
        connectionManager_ = std::make_unique<connections::service::connection_manager>(
            dbPath.toStdString(), masterPassword_.toStdString());

        // Check if the master password is valid for existing encrypted passwords
        if (!connectionManager_->verify_master_password()) {
            // There are encrypted passwords but the master password is wrong
            BOOST_LOG_SEV(lg(), debug) << "Master password required for encrypted passwords";

            // Prompt for master password (Unlock mode)
            MasterPasswordDialog dialog(MasterPasswordDialog::Unlock, this);
            if (dialog.exec() != QDialog::Accepted) {
                BOOST_LOG_SEV(lg(), debug) << "Master password entry cancelled";
                connectionManager_.reset();
                return false;
            }

            masterPassword_ = dialog.getPassword();

            // Re-create with the correct password
            connectionManager_ = std::make_unique<connections::service::connection_manager>(
                dbPath.toStdString(), masterPassword_.toStdString());

            // Verify again
            if (!connectionManager_->verify_master_password()) {
                BOOST_LOG_SEV(lg(), warn) << "Master password verification failed";
                MessageBoxHelper::warning(this, tr("Invalid Password"),
                    tr("The master password is incorrect."));
                masterPassword_.clear();
                connectionManager_.reset();
                return false;
            }

            BOOST_LOG_SEV(lg(), info) << "Master password verified successfully";
        } else if (masterPassword_.isEmpty()) {
            // Check if user has already been prompted and chose blank password
            QSettings settings;
            bool masterPasswordConfigured = settings.value(
                "connections/master_password_configured", false).toBool();

            if (!masterPasswordConfigured) {
                // Prompt to create master password
                // Note: we may have existing passwords encrypted with blank key
                BOOST_LOG_SEV(lg(), debug) << "Prompting for master password creation";

                MasterPasswordDialog dialog(MasterPasswordDialog::Create, this);
                if (dialog.exec() == QDialog::Accepted) {
                    QString newPassword = dialog.getNewPassword();

                    // Mark as configured regardless of whether blank or not
                    settings.setValue("connections/master_password_configured", true);

                    // Warn if blank password chosen
                    if (newPassword.isEmpty()) {
                        MessageBoxHelper::warning(this, tr("No Master Password"),
                            tr("You have chosen not to set a master password. "
                               "Saved passwords will not be encrypted securely.\n\n"
                               "You can set a master password later from the Connection Browser toolbar."));
                        BOOST_LOG_SEV(lg(), warn) << "User chose blank master password";
                    } else {
                        // Re-encrypt any existing passwords from blank to new password
                        try {
                            connectionManager_->change_master_password(newPassword.toStdString());
                            BOOST_LOG_SEV(lg(), info) << "Master password created and existing passwords re-encrypted";
                        } catch (const std::exception& e) {
                            BOOST_LOG_SEV(lg(), error) << "Failed to re-encrypt passwords with new master password: " << e.what();
                        }
                    }

                    masterPassword_ = newPassword;
                }
                // If cancelled, continue with empty password (user can set it later)
            }
        }

        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to initialize connection manager: "
                                   << e.what();
        MessageBoxHelper::critical(this, tr("Error"),
            tr("Failed to initialize connection manager: %1").arg(e.what()));
        return false;
    }
}

void MainWindow::onAboutTriggered() {
    if (aboutSubWindow_) {
        aboutSubWindow_->showNormal();
        mdiArea_->setActiveSubWindow(aboutSubWindow_);
        return;
    }

    auto* aboutWidget = new AboutDialog(clientManager_, this);
    aboutSubWindow_ = new DetachableMdiSubWindow();
    aboutSubWindow_->setWidget(aboutWidget);
    aboutSubWindow_->setWindowTitle(tr("About OreStudio"));
    aboutSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    aboutSubWindow_->resize(600, 700);

    connect(aboutSubWindow_, &QObject::destroyed, this, [this]() {
        allDetachableWindows_.removeOne(aboutSubWindow_);
        aboutSubWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(aboutSubWindow_);
    allDetachableWindows_.append(aboutSubWindow_);
    aboutSubWindow_->show();
}

void MainWindow::onMyAccountTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "My Account triggered";

    if (myAccountWindow_) {
        myAccountWindow_->showNormal();
        mdiArea_->setActiveSubWindow(myAccountWindow_);
        return;
    }

    auto* accountWidget = new MyAccountDialog(clientManager_, this);
    connect(accountWidget, &MyAccountDialog::viewSessionHistoryRequested,
            this, &MainWindow::onMySessionsTriggered);

    myAccountWindow_ = new DetachableMdiSubWindow();
    myAccountWindow_->setWidget(accountWidget);
    myAccountWindow_->setWindowTitle(tr("My Account"));
    myAccountWindow_->setAttribute(Qt::WA_DeleteOnClose);
    myAccountWindow_->resize(500, 400);

    connect(myAccountWindow_, &QObject::destroyed, this, [this]() {
        allDetachableWindows_.removeOne(myAccountWindow_);
        myAccountWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(myAccountWindow_);
    allDetachableWindows_.append(myAccountWindow_);
    myAccountWindow_->show();
}

void MainWindow::onMySessionsTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "My Sessions triggered";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Not connected, cannot show sessions";
        return;
    }

    if (mySessionsWindow_) {
        mySessionsWindow_->showNormal();
        mdiArea_->setActiveSubWindow(mySessionsWindow_);
        return;
    }

    const auto accountId = clientManager_->accountId();
    if (!accountId) {
        BOOST_LOG_SEV(lg(), warn) << "No account ID available";
        return;
    }

    const QString username = QString::fromStdString(clientManager_->currentUsername());

    auto* sessionWidget = new SessionHistoryDialog(clientManager_, this);
    sessionWidget->setAccount(*accountId, username);

    mySessionsWindow_ = new DetachableMdiSubWindow();
    mySessionsWindow_->setWidget(sessionWidget);
    mySessionsWindow_->setWindowTitle(tr("My Sessions - %1").arg(username));
    mySessionsWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));
    mySessionsWindow_->setAttribute(Qt::WA_DeleteOnClose);
    mySessionsWindow_->resize(900, 500);

    connect(mySessionsWindow_, &QObject::destroyed, this, [this]() {
        allDetachableWindows_.removeOne(mySessionsWindow_);
        mySessionsWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(mySessionsWindow_);
    allDetachableWindows_.append(mySessionsWindow_);
    mySessionsWindow_->show();
}

void MainWindow::onDetachAllTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Detach All triggered";

    for (const auto& detachableWindow : allDetachableWindows_) {
        if (detachableWindow && !detachableWindow->isDetached()) {
            detachableWindow->detach();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "All windows detached.";
}

void MainWindow::onDetachableWindowCreated(DetachableMdiSubWindow* window) {
    if (window) {
        allDetachableWindows_.append(window);
    }
}

void MainWindow::onDetachableWindowDestroyed(DetachableMdiSubWindow* window) {
    allDetachableWindows_.removeOne(window);
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
            const auto& detachableWindow = allDetachableWindows_[i];
            if (!detachableWindow)
                continue;
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

void MainWindow::setHttpBaseUrl(const std::string& url) {
    httpBaseUrl_ = url;
    BOOST_LOG_SEV(lg(), info) << "HTTP base URL set: " << url;
    // URL is passed to plugins via plugin_context at login time.
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

    if (!instanceName_.isEmpty())
        title += QString(" [%1]").arg(instanceName_);

    setWindowTitle(title);
    updateStatusBarFields();
    BOOST_LOG_SEV(lg(), debug) << "Window title updated: " << title.toStdString();
}

void MainWindow::updateStatusBarFields() {
    const QString normalChipStyle =
        "QWidget { border-left: 1px solid palette(mid);"
        " background: palette(alternateBase); }";
    const QString warningChipStyle =
        "QWidget { border-left: 1px solid palette(mid); background: #7a2000; }";

    const bool connected = clientManager_ && clientManager_->isConnected();

    // Derive display username (strip @tenant suffix) once for all chips.
    QString displayUser;
    if (!username_.empty()) {
        const QString fullUser = QString::fromStdString(username_);
        const int atIdx = fullUser.indexOf('@');
        displayUser = (atIdx >= 0) ? fullUser.left(atIdx) : fullUser;
    }

    // User chip
    if (connected) {
        userStatusNameLabel_->setText(displayUser.isEmpty() ? "—" : displayUser);
        userStatusWidget_->setToolTip("User: " + userStatusNameLabel_->text());
        userStatusWidget_->setStyleSheet(normalChipStyle);
        userStatusWidget_->setVisible(true);
    } else {
        userStatusWidget_->setVisible(false);
    }

    // Server/Environment chip
    if (connected) {
        const auto host = QString::fromStdString(clientManager_->connectedHost());
        const auto port = clientManager_->connectedPort();
        const QString label = activeConnectionName_.isEmpty()
            ? QString("%1:%2").arg(host).arg(port)
            : activeConnectionName_;
        serverStatusNameLabel_->setText(label);
        serverStatusWidget_->setToolTip(
            QString("Server: %1\nPort: %2\nUser: %3").arg(host).arg(port).arg(displayUser));
        serverStatusWidget_->setStyleSheet(normalChipStyle);
        serverStatusWidget_->setVisible(true);
    } else {
        serverStatusWidget_->setVisible(false);
    }

    // Derive tenant name from the @tenant suffix in username_, falling back to
    // the saved connection name when the username carries no tenant suffix.
    QString tenantName;
    if (!username_.empty()) {
        const QString fullUser = QString::fromStdString(username_);
        const int atIdx = fullUser.indexOf('@');
        if (atIdx >= 0) {
            tenantName = fullUser.mid(atIdx + 1);
        }
    }
    if (tenantName.isEmpty()) {
        tenantName = activeConnectionName_;
    }

    if (connected) {
        tenantStatusNameLabel_->setText(tenantName.isEmpty() ? "Root" : tenantName);
        tenantStatusWidget_->setToolTip("Tenant: " + tenantStatusNameLabel_->text());
        tenantStatusWidget_->setStyleSheet(normalChipStyle);
        tenantStatusWidget_->setVisible(true);
    } else {
        tenantStatusWidget_->setVisible(false);
    }

    if (connected) {
        if (!party_name_.isEmpty()) {
            partyStatusNameLabel_->setText(party_name_);
            partyStatusNameLabel_->setStyleSheet("");
            partyStatusWidget_->setToolTip("Party: " + party_name_);
            partyStatusWidget_->setStyleSheet(normalChipStyle);
        } else {
            partyStatusNameLabel_->setText("No Party");
            partyStatusNameLabel_->setStyleSheet("color: #ffccaa; font-weight: bold;");
            partyStatusWidget_->setToolTip("Party: No party assigned");
            partyStatusWidget_->setStyleSheet(warningChipStyle);
        }
        partyStatusWidget_->setVisible(true);
    } else {
        partyStatusWidget_->setVisible(false);
    }
}

void MainWindow::onConnectionBrowserTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Connection Browser action triggered";

    // If window already exists, bring it to front
    if (connectionBrowserWindow_) {
        connectionBrowserWindow_->show();
        connectionBrowserWindow_->raise();
        connectionBrowserWindow_->activateWindow();
        return;
    }

    // Initialize connection manager if not already done
    if (!initializeConnectionManager()) {
        return;
    }

    // Create the Connection Browser window
    auto* browserWidget = new ConnectionBrowserMdiWindow(connectionManager_.get(), this);

    // Set up test connection callback
    browserWidget->setTestCallback([this](const QString& host, int port,
                                           const QString& username, const QString& password) -> QString {
        if (!clientManager_) {
            return tr("Client manager not initialized");
        }

        // Use testConnection which creates a temporary client without affecting
        // main connection state or emitting signals
        auto result = clientManager_->testConnection(
            host.toStdString(),
            static_cast<std::uint16_t>(port),
            username.toStdString(),
            password.toStdString());

        if (result.success) {
            return QString(); // Empty = success
        }

        return result.error_message;
    });

    // Pass MDI area so dialogs opened from the browser become MDI sub-windows
    browserWidget->setMdiArea(mdiArea_, this, &allDetachableWindows_);

    // Connect signals
    connect(browserWidget, &ConnectionBrowserMdiWindow::statusChanged,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(browserWidget, &ConnectionBrowserMdiWindow::errorOccurred,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(browserWidget, &ConnectionBrowserMdiWindow::connectRequested,
            this, &MainWindow::onConnectionConnectRequested);
    connect(browserWidget, &ConnectionBrowserMdiWindow::environmentConnectRequested,
            this, &MainWindow::onEnvironmentConnectRequested);
    connect(browserWidget, &ConnectionBrowserMdiWindow::databasePurged,
            this, [this]() {
        // Reset master password configuration when database is purged
        QSettings settings;
        settings.setValue("connections/master_password_configured", false);
        masterPassword_.clear();
        BOOST_LOG_SEV(lg(), info) << "Master password configuration reset after database purge";
    });
    connect(browserWidget, &ConnectionBrowserMdiWindow::changeMasterPasswordRequested,
            this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Change master password requested from Connection Browser";

        if (!connectionManager_) {
            BOOST_LOG_SEV(lg(), warn) << "Connection manager not initialized";
            return;
        }

        MasterPasswordDialog dialog(MasterPasswordDialog::Change, this);
        if (dialog.exec() != QDialog::Accepted) {
            return;
        }

        QString currentPassword = dialog.getPassword();
        QString newPassword = dialog.getNewPassword();

        // Verify the current password
        if (currentPassword != masterPassword_) {
            MessageBoxHelper::warning(this, tr("Invalid Password"),
                tr("The current password is incorrect."));
            return;
        }

        try {
            // Re-encrypt all stored passwords with the new master password
            connectionManager_->change_master_password(newPassword.toStdString());

            // Update stored master password
            masterPassword_ = newPassword;

            // Mark as configured (in case user is setting password for first time)
            QSettings settings;
            settings.setValue("connections/master_password_configured", true);

            MessageBoxHelper::information(this, tr("Password Changed"),
                tr("Master password has been changed successfully."));
            BOOST_LOG_SEV(lg(), info) << "Master password changed successfully";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to change master password: " << e.what();
            MessageBoxHelper::critical(this, tr("Error"),
                tr("Failed to change master password: %1").arg(e.what()));
        }
    });

    // Create MDI sub-window
    connectionBrowserWindow_ = new DetachableMdiSubWindow();
    connectionBrowserWindow_->setWidget(browserWidget);
    connectionBrowserWindow_->setWindowTitle(tr("Connection Browser"));
    connectionBrowserWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ServerLink, IconUtils::DefaultIconColor));
    connectionBrowserWindow_->setAttribute(Qt::WA_DeleteOnClose);

    // Track the window
    allDetachableWindows_.append(connectionBrowserWindow_);

    // Clean up when closed
    connect(connectionBrowserWindow_, &QObject::destroyed, this, [this]() {
        allDetachableWindows_.removeOne(connectionBrowserWindow_);
        connectionBrowserWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(connectionBrowserWindow_);
    connectionBrowserWindow_->resize(browserWidget->sizeHint());
    connectionBrowserWindow_->show();

    BOOST_LOG_SEV(lg(), info) << "Connection Browser window opened";
}

void MainWindow::onConnectionConnectRequested(const boost::uuids::uuid& connectionId,
                                               const QString& connectionName) {

    BOOST_LOG_SEV(lg(), debug) << "Connect requested for connection: "
                               << boost::uuids::to_string(connectionId)
                               << ", name: " << connectionName.toStdString();

    // If already connected, ask user to disconnect first
    if (clientManager_ && clientManager_->isConnected()) {
        auto result = MessageBoxHelper::question(this,
            tr("Already Connected"),
            tr("You are already connected to a server. Disconnect and connect to '%1'?")
                .arg(connectionName),
            QMessageBox::Yes | QMessageBox::No);

        if (result != QMessageBox::Yes) {
            return;
        }

        performDisconnectCleanup();
    }

    if (!connectionManager_) {
        BOOST_LOG_SEV(lg(), error) << "Connection manager not initialized";
        return;
    }

    // Resolve the connection (host/port from environment if linked, decrypt password)
    connections::service::connection_manager::resolved_connection resolved;
    try {
        resolved = connectionManager_->resolve_connection(connectionId);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to resolve connection: " << e.what();
        MessageBoxHelper::critical(this, tr("Error"),
            tr("Could not find the selected connection."));
        return;
    }

    // Store the connection name for the window title
    activeConnectionName_ = connectionName;

    // Show login dialog pre-filled with connection details
    LoginDialogOptions options;
    options.host = QString::fromStdString(resolved.host);
    options.port = resolved.port;
    options.username = QString::fromStdString(resolved.username);
    if (!resolved.password.empty()) {
        options.password = QString::fromStdString(resolved.password);
    }
    options.connectionName = connectionName;
    options.showSavedConnections = false;  // Already selected a connection
    options.showSignUpButton = false;      // Connecting to known server

    showLoginDialog(options);
}

void MainWindow::onEnvironmentConnectRequested(const boost::uuids::uuid& environmentId,
                                               const QString& environmentName) {

    BOOST_LOG_SEV(lg(), debug) << "Connect requested for environment: "
                               << boost::uuids::to_string(environmentId)
                               << ", name: " << environmentName.toStdString();

    // If already connected, ask user to disconnect first
    if (clientManager_ && clientManager_->isConnected()) {
        auto result = MessageBoxHelper::question(this,
            tr("Already Connected"),
            tr("You are already connected to a server. Disconnect and connect via '%1'?")
                .arg(environmentName),
            QMessageBox::Yes | QMessageBox::No);

        if (result != QMessageBox::Yes) {
            return;
        }

        performDisconnectCleanup();
    }

    if (!connectionManager_) {
        BOOST_LOG_SEV(lg(), error) << "Connection manager not initialized";
        return;
    }

    // Look up the environment to get host/port
    auto env = connectionManager_->get_environment(environmentId);
    if (!env) {
        BOOST_LOG_SEV(lg(), error) << "Environment not found: "
                                   << boost::uuids::to_string(environmentId);
        MessageBoxHelper::critical(this, tr("Error"),
            tr("Could not find the selected environment."));
        return;
    }

    activeConnectionName_ = environmentName;

    // Show login dialog pre-filled with environment host/port; user enters credentials
    LoginDialogOptions options;
    options.host = QString::fromStdString(env->host);
    options.port = env->port;
    options.connectionName = environmentName;
    options.showSavedConnections = false;
    options.showSignUpButton = false;

    showLoginDialog(options);
}

void MainWindow::onLoginSuccess(const QString& username) {
    BOOST_LOG_SEV(lg(), info) << "Login succeeded for user: "
                               << username.toStdString();

    username_ = username.toStdString();
    party_name_ = clientManager_ ? clientManager_->currentPartyName() : QString();

    // Build plugin context and drive plugin lifecycle
    plugin_context ctx;
    ctx.main_window         = this;
    ctx.mdi_area            = mdiArea_;
    ctx.status_bar          = ui_->statusbar;
    ctx.client_manager      = clientManager_;
    ctx.image_cache         = imageCache_;
    ctx.change_reason_cache = changeReasonCache_;
    ctx.badge_cache         = badgeCache_;
    ctx.event_bus           = eventBus_;
    ctx.username            = username;
    ctx.http_base_url       = httpBaseUrl_;

    // Drive plugin lifecycle in load_order sequence
    adminPlugin_->on_login(ctx);
    computePlugin_->on_login(ctx);
    refdataPlugin_->on_login(ctx);
    legacyPlugin_->on_login(ctx);

    // Insert domain menus from all plugins before Help
    auto* helpAction = ui_->menuHelp->menuAction();
    for (auto* plugin : {static_cast<IPlugin*>(adminPlugin_.get()),
                         static_cast<IPlugin*>(computePlugin_.get()),
                         static_cast<IPlugin*>(refdataPlugin_.get()),
                         static_cast<IPlugin*>(legacyPlugin_.get())}) {
        for (auto* menu : plugin->create_menus()) {
            menuBar()->insertMenu(helpAction, menu);
            plugin_menus_.append(menu);
        }
    }

    updateWindowTitle();
    updateMenuState();

    // Warn if no party context — the account is misconfigured.
    // Under normal circumstances the server rejects logins with no party, so
    // this is a last-resort defensive check.
    if (clientManager_ && clientManager_->isConnected() &&
        party_name_.isEmpty()) {
        MessageBoxHelper::warning(this, "No Party Assigned",
            "Your account has no party context.\n\n"
            "This is a configuration error — please contact your tenant "
            "administrator to assign your account to a party before continuing.");
    }
}

void MainWindow::showSignUpDialog(const QString& host, int port) {
    auto* signupWidget = new SignUpDialog();
    signupWidget->setClientManager(clientManager_);
    signupWidget->setServer(host);
    signupWidget->setPort(port);

    auto* signupWindow = new DetachableMdiSubWindow(this);
    signupWindow->setWidget(signupWidget);
    signupWindow->setWindowTitle(tr("Create Account"));
    signupWindow->setAttribute(Qt::WA_DeleteOnClose);
    signupWindow->resize(400, 620);
    signupWindow->setWindowFlags(Qt::FramelessWindowHint);

    mdiArea_->addSubWindow(signupWindow);
    signupWindow->show();

    QPoint center = mdiArea_->viewport()->rect().center();
    signupWindow->move(center.x() - signupWindow->width() / 2,
                      center.y() - signupWindow->height() / 2);

    connect(signupWidget, &SignUpDialog::closeRequested,
            signupWindow, &QWidget::close);

    // When auto-login after signup succeeds, update application state
    connect(signupWidget, &SignUpDialog::loginSucceeded,
            this, [this](const QString& username) {
        onLoginSuccess(username);
        ui_->statusbar->showMessage(
            QString("Account '%1' created and logged in successfully.").arg(username));
    });

    // When user wants to go back to login
    connect(signupWidget, &SignUpDialog::loginRequested,
            this, [this, signupWindow]() {
        signupWindow->close();
        onModernLoginTriggered();
    });

    allDetachableWindows_.append(signupWindow);
    connect(signupWindow, &QObject::destroyed, this, [this, signupWindow]() {
        allDetachableWindows_.removeOne(signupWindow);
    });
}

void MainWindow::showSystemProvisionerWizard(
    const QString& username, const QString& password) {
    BOOST_LOG_SEV(lg(), info) << "Showing System Provisioner Wizard (bootstrap mode detected)";

    auto* wizard = new SystemProvisionerWizard(clientManager_, this);

    // Pre-fill admin form with login credentials if available
    if (!username.isEmpty() || !password.isEmpty()) {
        wizard->setAdminCredentials(username, {}, password);
    }
    wizard->setWindowModality(Qt::ApplicationModal);
    wizard->setAttribute(Qt::WA_DeleteOnClose);

    // Connect completion signal - on success, proceed with normal flow
    connect(wizard, &SystemProvisionerWizard::provisioningCompleted,
            this, [this](const QString& username) {
        BOOST_LOG_SEV(lg(), info) << "System provisioning completed, user logged in as: "
                                  << username.toStdString();
        ui_->statusbar->showMessage(
            tr("System provisioned. Logged in as administrator '%1'.").arg(username));

        // The system is now provisioned and user is already logged in from provisioning
        // Call onLoginSuccess to properly initialize the application state
        onLoginSuccess(username);
    });

    // Connect failure signal
    connect(wizard, &SystemProvisionerWizard::provisioningFailed,
            this, [this](const QString& errorMessage) {
        BOOST_LOG_SEV(lg(), error) << "System provisioning failed: "
                                   << errorMessage.toStdString();
        ui_->statusbar->showMessage(tr("Provisioning failed: %1").arg(errorMessage));

        // Disconnect and allow retry
        if (clientManager_) {
            clientManager_->disconnect();
        }
    });

    wizard->show();
}

void MainWindow::showTenantProvisioningWizard() {
    BOOST_LOG_SEV(lg(), info) << "Showing Tenant Provisioning Wizard (tenant bootstrap mode)";

    auto* wizard = new TenantProvisioningWizard(clientManager_, this);
    wizard->setWindowModality(Qt::ApplicationModal);
    wizard->setAttribute(Qt::WA_DeleteOnClose);

    connect(wizard, &TenantProvisioningWizard::provisioningCompleted,
            this, [this]() {
        BOOST_LOG_SEV(lg(), info) << "Tenant provisioning wizard completed";
        ui_->statusbar->showMessage(
            tr("Tenant setup completed successfully."));
    });

    wizard->show();
}

void MainWindow::showPartyProvisioningWizard() {
    BOOST_LOG_SEV(lg(), info) << "Showing Party Provisioning Wizard (party setup mode)";

    auto* wizard = new PartyProvisioningWizard(clientManager_, this);
    wizard->setWindowModality(Qt::ApplicationModal);
    wizard->setAttribute(Qt::WA_DeleteOnClose);

    connect(wizard, &PartyProvisioningWizard::provisioningCompleted,
            this, [this]() {
        BOOST_LOG_SEV(lg(), info) << "Party provisioning wizard completed";
        ui_->statusbar->showMessage(
            tr("Party setup completed successfully."));
    });

    wizard->show();
}

void MainWindow::showLoginDialog() {
    showLoginDialog(LoginDialogOptions{});
}

void MainWindow::showLoginDialog(const LoginDialogOptions& options) {
    BOOST_LOG_SEV(lg(), debug) << "Showing login dialog";

    auto* loginWidget = new LoginDialog();
    loginWidget->setClientManager(clientManager_);
    loginWidget->setImageCache(imageCache_);

    // Pre-fill connection details if provided
    if (!options.host.isEmpty()) {
        loginWidget->setServer(options.host);
    }
    if (options.port > 0) {
        loginWidget->setPort(options.port);
    }
    if (!options.username.isEmpty()) {
        loginWidget->setUsername(options.username);
    }
    if (!options.password.isEmpty()) {
        loginWidget->setPassword(options.password);
    }

    // Store connection name for status messages
    const QString connectionName = options.connectionName;

    // Create MDI sub-window
    auto* subWindow = new DetachableMdiSubWindow(this);
    subWindow->setWidget(loginWidget);
    subWindow->setWindowTitle(tr("Login"));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(400, 520);
    subWindow->setWindowFlags(Qt::FramelessWindowHint);

    mdiArea_->addSubWindow(subWindow);
    subWindow->show();

    // Center in MDI area
    QPoint center = mdiArea_->viewport()->rect().center();
    subWindow->move(center.x() - subWindow->width() / 2,
                    center.y() - subWindow->height() / 2);

    // Connect close signal
    connect(loginWidget, &LoginDialog::closeRequested, subWindow, &QWidget::close);

    // Connect HTTP base URL discovery signal (from NATS service discovery)
    connect(loginWidget, &LoginDialog::httpBaseUrlDiscovered,
            this, [this](const QString& url) {
        setHttpBaseUrl(url.toStdString());
    });

    // Connect login success signal
    connect(loginWidget, &LoginDialog::loginSucceeded,
            this, [this, connectionName](const QString& username) {
        onLoginSuccess(username);
        if (!connectionName.isEmpty()) {
            ui_->statusbar->showMessage(tr("Connected to %1").arg(connectionName));
        } else {
            ui_->statusbar->showMessage(tr("Successfully connected and logged in."));
        }
    });

    // Connect bootstrap mode signal - pass login credentials to pre-fill wizard
    connect(loginWidget, &LoginDialog::bootstrapModeDetected,
            this, [this, loginWidget]() {
        showSystemProvisionerWizard(
            loginWidget->getUsername(), loginWidget->getPassword());
    });

    // Connect tenant bootstrap mode signal - show tenant provisioning wizard
    connect(loginWidget, &LoginDialog::tenantBootstrapDetected,
            this, [this]() {
        showTenantProvisioningWizard();
    });

    // Connect party setup mode signal - show party provisioning wizard
    connect(loginWidget, &LoginDialog::partySetupDetected,
            this, [this]() {
        showPartyProvisioningWizard();
    });

    // Connect sign up request if enabled
    if (options.showSignUpButton) {
        connect(loginWidget, &LoginDialog::signUpRequested,
                this, [this, subWindow, loginWidget]() {
            const QString host = loginWidget->getServer();
            const int port = loginWidget->getPort();
            subWindow->close();
            showSignUpDialog(host, port);
        });
    }

    // Populate quick-connect combo with environments (fills host+port) and
    // full connections (fills all fields including credentials).
    if (options.showSavedConnections && initializeConnectionManager() && connectionManager_) {
        QList<LoginDialog::QuickConnectItem> items;

        // Environments: selecting fills host+port only; user types credentials
        for (const auto& env : connectionManager_->get_all_environments()) {
            LoginDialog::QuickConnectItem it;
            it.type = LoginDialog::QuickConnectItem::Type::Environment;
            it.name = QString::fromStdString(env.name);
            it.subtitle = QString("%1:%2")
                .arg(QString::fromStdString(env.host))
                .arg(env.port);
            items.append(it);
        }

        // Connections: selecting fills all fields via resolve_connection()
        for (const auto& conn : connectionManager_->get_all_connections()) {
            LoginDialog::QuickConnectItem it;
            it.type = LoginDialog::QuickConnectItem::Type::Connection;
            it.name = QString::fromStdString(conn.name);
            it.subtitle = QString::fromStdString(conn.username);
            items.append(it);
        }

        if (!items.isEmpty()) {
            loginWidget->setQuickConnectItems(items);

            // Environment selected: fill host+port, credentials stay editable
            connect(loginWidget, &LoginDialog::environmentSelected,
                    this, [this, loginWidget](const QString& name) {
                if (!connectionManager_) return;
                for (const auto& env : connectionManager_->get_all_environments()) {
                    if (QString::fromStdString(env.name) == name) {
                        loginWidget->setServer(QString::fromStdString(env.host));
                        loginWidget->setPort(env.port);
                        loginWidget->setSubjectPrefix(
                            QString::fromStdString(env.subject_prefix));
                        break;
                    }
                }
            });

            // Connection selected: resolve and fill all fields
            connect(loginWidget, &LoginDialog::connectionSelected,
                    this, [this, loginWidget](const QString& name) {
                if (!connectionManager_) return;
                for (const auto& conn : connectionManager_->get_all_connections()) {
                    if (QString::fromStdString(conn.name) == name) {
                        try {
                            auto resolved = connectionManager_->resolve_connection(conn.id);
                            loginWidget->setServer(QString::fromStdString(resolved.host));
                            loginWidget->setPort(resolved.port);
                            if (!resolved.subject_prefix.empty()) {
                                loginWidget->setSubjectPrefix(
                                    QString::fromStdString(resolved.subject_prefix));
                            }
                            loginWidget->setUsername(
                                QString::fromStdString(resolved.username));
                            if (!resolved.password.empty()) {
                                loginWidget->setPassword(
                                    QString::fromStdString(resolved.password));
                            }
                        } catch (const std::exception& e) {
                            using namespace ores::logging;
                            BOOST_LOG_SEV(lg(), error)
                                << "Failed to resolve connection: " << e.what();
                        }
                        break;
                    }
                }
            });
        }
    }

    // Track window
    allDetachableWindows_.append(subWindow);
    connect(subWindow, &QObject::destroyed, this, [this, subWindow, connectionName]() {
        allDetachableWindows_.removeOne(subWindow);
        if (!connectionName.isEmpty()) {
            activeConnectionName_.clear();
        }
    });
}

void MainWindow::onModernLoginTriggered() {
    BOOST_LOG_SEV(lg(), debug) << "Modern Login action triggered";

    // If already connected, ask user to disconnect first
    if (clientManager_ && clientManager_->isConnected()) {
        auto result = MessageBoxHelper::question(this,
            tr("Already Connected"),
            tr("You are already connected to a server. Disconnect and connect to a new server?"),
            QMessageBox::Yes | QMessageBox::No);

        if (result != QMessageBox::Yes) {
            return;
        }

        performDisconnectCleanup();
    }

    // Show login dialog with default options (saved connections, sign up enabled)
    showLoginDialog();
}

}
