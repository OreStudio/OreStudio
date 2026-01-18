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
#include "ores.qt/SignUpDialog.hpp"
#include "ores.qt/MyAccountDialog.hpp"
#include "ores.qt/SessionHistoryDialog.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/CountryController.hpp"
#include "ores.qt/AccountController.hpp"
#include "ores.qt/RoleController.hpp"
#include "ores.qt/FeatureFlagController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/OriginDimensionController.hpp"
#include "ores.qt/NatureDimensionController.hpp"
#include "ores.qt/TreatmentDimensionController.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"
#include "ores.qt/DataDomainController.hpp"
#include "ores.qt/SubjectAreaController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/CodingSchemeController.hpp"
#include "ores.qt/MethodologyController.hpp"
#include "ores.qt/DatasetController.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"
#include "ores.qt/EventViewerDialog.hpp"
#include "ores.qt/TelemetryMdiWindow.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/TelemetrySettingsDialog.hpp"
#include "ores.qt/ConnectionBrowserMdiWindow.hpp"
#include "ores.qt/MasterPasswordDialog.hpp"
#include "ores.comms/eventing/connection_events.hpp"
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
    systemTrayIcon_(nullptr), trayContextMenu_(nullptr),
    instanceColorIndicator_(nullptr), eventViewerWindow_(nullptr),
    telemetryViewerWindow_(nullptr) {

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
    ui_->CountriesAction->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_globe_20_regular.svg", iconColor));
    ui_->ActionAbout->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_star_20_regular.svg", iconColor));
    ui_->ActionAccounts->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));
    ui_->ActionRoles->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_lock_closed_20_regular.svg", iconColor));
    ui_->ActionFeatureFlags->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_flag_20_regular.svg", iconColor));
    ui_->ActionChangeReasonCategories->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));
    ui_->ActionChangeReasons->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_note_edit_20_regular.svg", iconColor));
    ui_->ActionOriginDimensions->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    ui_->ActionNatureDimensions->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    ui_->ActionTreatmentDimensions->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    ui_->ActionCodingSchemeAuthorityTypes->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));
    ui_->ActionDataDomains->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor));
    ui_->ActionCodingSchemes->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_code_20_regular.svg", iconColor));
    ui_->ActionMethodologies->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_book_20_regular.svg", iconColor));
    ui_->ActionDatasets->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
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
    ui_->ActionConnectionBrowser->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_server_link_20_regular.svg", iconColor));

    // Create record icons - regular (gray) for off, filled (red) for on
    recordOffIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_record_20_regular.svg", iconColor);
    recordOnIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_record_20_filled.svg", QColor(220, 80, 80)); // Red for recording
    ui_->ActionRecordSession->setIcon(recordOffIcon_);
    ui_->ActionEventViewer->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_document_code_16_regular.svg", iconColor));
    ui_->ActionTelemetryViewer->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_document_table_20_regular.svg", iconColor));

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

        const QColor iconColor(220, 220, 220);
        telemetryViewerWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_document_table_20_regular.svg", iconColor));

        // Track window destruction
        connect(telemetryViewerWindow_, &QObject::destroyed, this, [this]() {
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

    // Load caches when connected
    connect(clientManager_, &ClientManager::connected, this, [this]() {
        imageCache_->loadAll();
        // Preload all available images for the flag selector to avoid on-demand loading delay
        if (!imageCache_->hasImageList()) {
            imageCache_->loadImageList();
        } else {
            imageCache_->loadAllAvailableImages();
        }

        // Load change reasons for entity dialogs
        changeReasonCache_->loadAll();
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

    // Connect Countries action to controller
    connect(ui_->CountriesAction, &QAction::triggered, this, [this]() {
        if (countryController_)
            countryController_->showListWindow();
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

    // Connect Change Reason Categories action to controller (admin only)
    connect(ui_->ActionChangeReasonCategories, &QAction::triggered, this, [this]() {
        if (changeReasonCategoryController_)
            changeReasonCategoryController_->showListWindow();
    });

    // Connect Change Reasons action to controller (admin only)
    connect(ui_->ActionChangeReasons, &QAction::triggered, this, [this]() {
        if (changeReasonController_)
            changeReasonController_->showListWindow();
    });

    // Connect Origin Dimensions action to controller
    connect(ui_->ActionOriginDimensions, &QAction::triggered, this, [this]() {
        if (originDimensionController_)
            originDimensionController_->showListWindow();
    });

    // Connect Nature Dimensions action to controller
    connect(ui_->ActionNatureDimensions, &QAction::triggered, this, [this]() {
        if (natureDimensionController_)
            natureDimensionController_->showListWindow();
    });

    // Connect Treatment Dimensions action to controller
    connect(ui_->ActionTreatmentDimensions, &QAction::triggered, this, [this]() {
        if (treatmentDimensionController_)
            treatmentDimensionController_->showListWindow();
    });

    // Connect Coding Scheme Authority Types action to controller
    connect(ui_->ActionCodingSchemeAuthorityTypes, &QAction::triggered, this, [this]() {
        if (codingSchemeAuthorityTypeController_)
            codingSchemeAuthorityTypeController_->showListWindow();
    });

    // Connect Data Domains action to controller
    connect(ui_->ActionDataDomains, &QAction::triggered, this, [this]() {
        if (dataDomainController_)
            dataDomainController_->showListWindow();
    });

    // Connect Subject Areas action to controller
    connect(ui_->ActionSubjectAreas, &QAction::triggered, this, [this]() {
        if (subjectAreaController_)
            subjectAreaController_->showListWindow();
    });

    // Connect Catalogs action to controller
    connect(ui_->ActionCatalogs, &QAction::triggered, this, [this]() {
        if (catalogController_)
            catalogController_->showListWindow();
    });

    // Connect Coding Schemes action to controller
    connect(ui_->ActionCodingSchemes, &QAction::triggered, this, [this]() {
        if (codingSchemeController_)
            codingSchemeController_->showListWindow();
    });

    // Connect Methodologies action to controller
    connect(ui_->ActionMethodologies, &QAction::triggered, this, [this]() {
        if (methodologyController_)
            methodologyController_->showListWindow();
    });

    // Connect Datasets action to controller
    connect(ui_->ActionDatasets, &QAction::triggered, this, [this]() {
        if (datasetController_)
            datasetController_->showListWindow();
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
    // Delegate to the modeless login dialog implementation
    onModernLoginTriggered();
}

void MainWindow::updateMenuState() {
    const bool isConnected = clientManager_ && clientManager_->isConnected();

    // Enable/disable menu actions based on connection state
    ui_->CurrenciesAction->setEnabled(isConnected);
    ui_->CountriesAction->setEnabled(isConnected);

    // Enable/disable connect and disconnect actions
    ui_->ActionConnect->setEnabled(!isConnected);
    ui_->ActionDisconnect->setEnabled(isConnected);

    // System menu enabled when connected - permission checks happen server-side via RBAC
    ui_->menuSystem->menuAction()->setEnabled(isConnected);
    ui_->ActionAccounts->setEnabled(isConnected);

    // Data Quality menu enabled when connected
    ui_->menuDataQuality->menuAction()->setEnabled(isConnected);
    ui_->ActionRoles->setEnabled(isConnected);
    ui_->ActionFeatureFlags->setEnabled(isConnected);
    ui_->ActionChangeReasonCategories->setEnabled(isConnected);
    ui_->ActionChangeReasons->setEnabled(isConnected);
    ui_->ActionOriginDimensions->setEnabled(isConnected);
    ui_->ActionNatureDimensions->setEnabled(isConnected);
    ui_->ActionTreatmentDimensions->setEnabled(isConnected);
    ui_->ActionCodingSchemeAuthorityTypes->setEnabled(isConnected);
    ui_->ActionDataDomains->setEnabled(isConnected);
    ui_->ActionSubjectAreas->setEnabled(isConnected);
    ui_->ActionCatalogs->setEnabled(isConnected);
    ui_->ActionCodingSchemes->setEnabled(isConnected);
    ui_->ActionMethodologies->setEnabled(isConnected);
    ui_->ActionDatasets->setEnabled(isConnected);

    // My Account and My Sessions menu items are enabled when connected
    ui_->ActionMyAccount->setEnabled(isConnected);
    ui_->ActionMySessions->setEnabled(isConnected);

    // Telemetry viewer needs connection to load sessions/logs
    ui_->ActionTelemetryViewer->setEnabled(isConnected);

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
        this, mdiArea_, clientManager_, imageCache_, changeReasonCache_,
        QString::fromStdString(username_), this);

    // Connect controller signals to status bar and window lifecycle
    connect(currencyController_.get(), &CurrencyController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(currencyController_.get(), &CurrencyController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(currencyController_.get(), &CurrencyController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(currencyController_.get(), &CurrencyController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create country controller
    countryController_ = std::make_unique<CountryController>(
        this, mdiArea_, clientManager_, imageCache_, changeReasonCache_,
        QString::fromStdString(username_), this);

    // Connect country controller signals to status bar and window lifecycle
    connect(countryController_.get(), &CountryController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(countryController_.get(), &CountryController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(countryController_.get(), &CountryController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(countryController_.get(), &CountryController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create account controller (admin only functionality)
    accountController_ = std::make_unique<AccountController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect account controller signals to status bar and window lifecycle
    connect(accountController_.get(), &AccountController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(accountController_.get(), &AccountController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(accountController_.get(), &AccountController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(accountController_.get(), &AccountController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create role controller (admin only functionality)
    roleController_ = std::make_unique<RoleController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect role controller signals to status bar and window lifecycle
    connect(roleController_.get(), &RoleController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(roleController_.get(), &RoleController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(roleController_.get(), &RoleController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(roleController_.get(), &RoleController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create feature flag controller (admin only functionality)
    featureFlagController_ = std::make_unique<FeatureFlagController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect feature flag controller signals to status bar and window lifecycle
    connect(featureFlagController_.get(), &FeatureFlagController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(featureFlagController_.get(), &FeatureFlagController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(featureFlagController_.get(), &FeatureFlagController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(featureFlagController_.get(), &FeatureFlagController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create change reason category controller (admin only functionality)
    changeReasonCategoryController_ = std::make_unique<ChangeReasonCategoryController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect change reason category controller signals to status bar and window lifecycle
    connect(changeReasonCategoryController_.get(), &ChangeReasonCategoryController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(changeReasonCategoryController_.get(), &ChangeReasonCategoryController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(changeReasonCategoryController_.get(), &ChangeReasonCategoryController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(changeReasonCategoryController_.get(), &ChangeReasonCategoryController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create change reason controller (admin only functionality)
    changeReasonController_ = std::make_unique<ChangeReasonController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_),
        changeReasonCache_, this);

    // Connect change reason controller signals to status bar and window lifecycle
    connect(changeReasonController_.get(), &ChangeReasonController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(changeReasonController_.get(), &ChangeReasonController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(changeReasonController_.get(), &ChangeReasonController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(changeReasonController_.get(), &ChangeReasonController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create origin dimension controller
    originDimensionController_ = std::make_unique<OriginDimensionController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect origin dimension controller signals to status bar and window lifecycle
    connect(originDimensionController_.get(), &OriginDimensionController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(originDimensionController_.get(), &OriginDimensionController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(originDimensionController_.get(), &OriginDimensionController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(originDimensionController_.get(), &OriginDimensionController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create nature dimension controller
    natureDimensionController_ = std::make_unique<NatureDimensionController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect nature dimension controller signals to status bar and window lifecycle
    connect(natureDimensionController_.get(), &NatureDimensionController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(natureDimensionController_.get(), &NatureDimensionController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(natureDimensionController_.get(), &NatureDimensionController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(natureDimensionController_.get(), &NatureDimensionController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create treatment dimension controller
    treatmentDimensionController_ = std::make_unique<TreatmentDimensionController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect treatment dimension controller signals to status bar and window lifecycle
    connect(treatmentDimensionController_.get(), &TreatmentDimensionController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(treatmentDimensionController_.get(), &TreatmentDimensionController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(treatmentDimensionController_.get(), &TreatmentDimensionController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(treatmentDimensionController_.get(), &TreatmentDimensionController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create coding scheme authority type controller
    codingSchemeAuthorityTypeController_ = std::make_unique<CodingSchemeAuthorityTypeController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect coding scheme authority type controller signals to status bar and window lifecycle
    connect(codingSchemeAuthorityTypeController_.get(), &CodingSchemeAuthorityTypeController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(codingSchemeAuthorityTypeController_.get(), &CodingSchemeAuthorityTypeController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(codingSchemeAuthorityTypeController_.get(), &CodingSchemeAuthorityTypeController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(codingSchemeAuthorityTypeController_.get(), &CodingSchemeAuthorityTypeController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create data domain controller
    dataDomainController_ = std::make_unique<DataDomainController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect data domain controller signals to status bar and window lifecycle
    connect(dataDomainController_.get(), &DataDomainController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(dataDomainController_.get(), &DataDomainController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(dataDomainController_.get(), &DataDomainController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(dataDomainController_.get(), &DataDomainController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create subject area controller
    subjectAreaController_ = std::make_unique<SubjectAreaController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect subject area controller signals to status bar and window lifecycle
    connect(subjectAreaController_.get(), &SubjectAreaController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(subjectAreaController_.get(), &SubjectAreaController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(subjectAreaController_.get(), &SubjectAreaController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(subjectAreaController_.get(), &SubjectAreaController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create catalog controller
    catalogController_ = std::make_unique<CatalogController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect catalog controller signals to status bar and window lifecycle
    connect(catalogController_.get(), &CatalogController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(catalogController_.get(), &CatalogController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(catalogController_.get(), &CatalogController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(catalogController_.get(), &CatalogController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create coding scheme controller
    codingSchemeController_ = std::make_unique<CodingSchemeController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect coding scheme controller signals to status bar and window lifecycle
    connect(codingSchemeController_.get(), &CodingSchemeController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(codingSchemeController_.get(), &CodingSchemeController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(codingSchemeController_.get(), &CodingSchemeController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(codingSchemeController_.get(), &CodingSchemeController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create methodology controller
    methodologyController_ = std::make_unique<MethodologyController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect methodology controller signals to status bar and window lifecycle
    connect(methodologyController_.get(), &MethodologyController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(methodologyController_.get(), &MethodologyController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(methodologyController_.get(), &MethodologyController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(methodologyController_.get(), &MethodologyController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

    // Create dataset controller
    datasetController_ = std::make_unique<DatasetController>(
        this, mdiArea_, clientManager_, QString::fromStdString(username_), this);

    // Connect dataset controller signals to status bar and window lifecycle
    connect(datasetController_.get(), &DatasetController::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(datasetController_.get(), &DatasetController::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(datasetController_.get(), &DatasetController::detachableWindowCreated,
            this, &MainWindow::onDetachableWindowCreated);
    connect(datasetController_.get(), &DatasetController::detachableWindowDestroyed,
            this, &MainWindow::onDetachableWindowDestroyed);

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
        if (!username_.empty()) {
            // Use active connection name if available, otherwise server address
            QString connectionInfo = !activeConnectionName_.isEmpty()
                ? activeConnectionName_
                : QString::fromStdString(clientManager_->serverAddress());
            title += QString(" - %1@%2")
                .arg(QString::fromStdString(username_))
                .arg(connectionInfo);
        } else {
            QString serverInfo = QString::fromStdString(clientManager_->serverAddress());
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
    const QColor windowIconColor(220, 220, 220);
    connectionBrowserWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_server_link_20_regular.svg", windowIconColor));
    connectionBrowserWindow_->setAttribute(Qt::WA_DeleteOnClose);

    // Track the window
    allDetachableWindows_.append(connectionBrowserWindow_);

    // Clean up when closed
    connect(connectionBrowserWindow_, &QObject::destroyed, this, [this]() {
        allDetachableWindows_.removeOne(connectionBrowserWindow_);
        connectionBrowserWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(connectionBrowserWindow_);
    connectionBrowserWindow_->show();

    BOOST_LOG_SEV(lg(), info) << "Connection Browser window opened";
}

void MainWindow::onConnectionConnectRequested(const boost::uuids::uuid& environmentId,
                                               const QString& connectionName) {

    BOOST_LOG_SEV(lg(), debug) << "Connect requested for environment: "
                               << boost::uuids::to_string(environmentId)
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

    // Get the environment details
    auto env = connectionManager_->get_environment(environmentId);
    if (!env) {
        MessageBoxHelper::critical(this, tr("Error"),
            tr("Could not find the selected connection."));
        return;
    }

    // Get the decrypted password
    std::string password;
    try {
        password = connectionManager_->get_password(environmentId);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get password: " << e.what();
        // Password might be empty or decryption failed - user will need to enter it
    }

    // Store the connection name for the window title
    activeConnectionName_ = connectionName;

    // Open login dialog pre-filled with connection details
    auto* loginDialog = new LoginDialog();
    loginDialog->setClientManager(clientManager_);
    loginDialog->setServer(QString::fromStdString(env->host));
    loginDialog->setPort(env->port);
    loginDialog->setUsername(QString::fromStdString(env->username));
    if (!password.empty()) {
        loginDialog->setPassword(QString::fromStdString(password));
    }

    auto* subWindow = new DetachableMdiSubWindow(this);
    subWindow->setWidget(loginDialog);
    subWindow->setWindowTitle(tr("Login"));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(400, 520);
    subWindow->setWindowFlags(Qt::FramelessWindowHint);

    mdiArea_->addSubWindow(subWindow);
    subWindow->show();

    QPoint center = mdiArea_->viewport()->rect().center();
    subWindow->move(center.x() - subWindow->width() / 2,
                    center.y() - subWindow->height() / 2);

    connect(loginDialog, &LoginDialog::closeRequested, subWindow, &QWidget::close);

    connect(loginDialog, &LoginDialog::loginSucceeded,
            this, [this, connectionName](const QString& username) {
        onLoginSuccess(username);
        ui_->statusbar->showMessage(tr("Connected to %1").arg(connectionName));
    });

    allDetachableWindows_.append(subWindow);
    connect(subWindow, &QObject::destroyed, this, [this, subWindow]() {
        allDetachableWindows_.removeOne(subWindow);
        activeConnectionName_.clear();
    });
}

void MainWindow::onLoginSuccess(const QString& username) {
    BOOST_LOG_SEV(lg(), info) << "Login succeeded for user: "
                               << username.toStdString();

    username_ = username.toStdString();

    // Update controllers with new username
    if (currencyController_) {
        currencyController_->setUsername(username);
    }
    if (countryController_) {
        countryController_->setUsername(username);
    }
    if (accountController_) {
        accountController_->setUsername(username);
    }
    if (roleController_) {
        roleController_->setUsername(username);
    }
    if (featureFlagController_) {
        featureFlagController_->setUsername(username);
    }
    if (changeReasonCategoryController_) {
        changeReasonCategoryController_->setUsername(username);
    }
    if (changeReasonController_) {
        changeReasonController_->setUsername(username);
    }
    if (originDimensionController_) {
        originDimensionController_->setUsername(username);
    }
    if (natureDimensionController_) {
        natureDimensionController_->setUsername(username);
    }
    if (treatmentDimensionController_) {
        treatmentDimensionController_->setUsername(username);
    }
    if (codingSchemeAuthorityTypeController_) {
        codingSchemeAuthorityTypeController_->setUsername(username);
    }
    if (dataDomainController_) {
        dataDomainController_->setUsername(username);
    }
    if (subjectAreaController_) {
        subjectAreaController_->setUsername(username);
    }
    if (catalogController_) {
        catalogController_->setUsername(username);
    }
    if (codingSchemeController_) {
        codingSchemeController_->setUsername(username);
    }
    if (methodologyController_) {
        methodologyController_->setUsername(username);
    }
    if (datasetController_) {
        datasetController_->setUsername(username);
    }

    updateWindowTitle();
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

    auto* loginWidget = new LoginDialog();
    loginWidget->setClientManager(clientManager_);

    // Create MDI sub-window
    auto* subWindow = new DetachableMdiSubWindow(this);
    subWindow->setWidget(loginWidget);
    subWindow->setWindowTitle(tr("Login"));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(400, 520);

    // Remove window frame for cleaner look
    subWindow->setWindowFlags(Qt::FramelessWindowHint);

    mdiArea_->addSubWindow(subWindow);
    subWindow->show();

    // Center in MDI area
    QPoint center = mdiArea_->viewport()->rect().center();
    subWindow->move(center.x() - subWindow->width() / 2,
                    center.y() - subWindow->height() / 2);

    // Connect close signal
    connect(loginWidget, &LoginDialog::closeRequested, subWindow, &QWidget::close);

    // Connect login success signal to update application state
    connect(loginWidget, &LoginDialog::loginSucceeded,
            this, [this](const QString& username) {
        onLoginSuccess(username);
        ui_->statusbar->showMessage("Successfully connected and logged in.");
    });

    // Connect sign up request to open registration widget
    connect(loginWidget, &LoginDialog::signUpRequested,
            this, [this, subWindow, loginWidget]() {
        const QString host = loginWidget->getServer();
        const int port = loginWidget->getPort();
        subWindow->close();
        showSignUpDialog(host, port);
    });

    // Populate saved connections if connection manager is available
    if (initializeConnectionManager() && connectionManager_) {
        auto environments = connectionManager_->get_all_environments();
        QStringList connectionNames;
        for (const auto& env : environments) {
            connectionNames << QString::fromStdString(env.name);
        }
        loginWidget->setSavedConnections(connectionNames);

        // Connect saved connection selection
        connect(loginWidget, &LoginDialog::savedConnectionSelected,
                this, [this, loginWidget](const QString& name) {
            if (!connectionManager_) return;

            auto environments = connectionManager_->get_all_environments();
            for (const auto& env : environments) {
                if (QString::fromStdString(env.name) == name) {
                    loginWidget->setServer(QString::fromStdString(env.host));
                    loginWidget->setPort(env.port);
                    loginWidget->setUsername(QString::fromStdString(env.username));

                    // Get the saved password (empty if not saved)
                    auto password = connectionManager_->get_password(env.id);
                    if (!password.empty()) {
                        loginWidget->setPassword(QString::fromStdString(password));
                    }
                    break;
                }
            }
        });
    }

    // Track window
    allDetachableWindows_.append(subWindow);
    connect(subWindow, &QObject::destroyed, this, [this, subWindow]() {
        allDetachableWindows_.removeOne(subWindow);
    });
}

}
