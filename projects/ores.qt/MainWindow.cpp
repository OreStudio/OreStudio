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
#include <QTimer>
#include <QApplication>
#include <QScreen>
#include <QMdiSubWindow>
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QFile>
#include <QFont>
#include <QIcon>
#include "ui_MainWindow.h"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/LoginDialog.hpp"
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/CurrencyHistoryMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/CurrencyDetailPanel.hpp" // Include the header for CurrencyDetailPanel
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/AboutDialog.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(nullptr),
    activeCurrencyWindow_(nullptr), selectionCount_(0),
    currencyListWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating the main window.";
    ui_->setupUi(this);

    // Create MDI area with proper parent
    mdiArea_ = new MdiAreaWithBackground(this);

    // Set application icon
    setWindowIcon(QIcon("modern-icon.png"));

    // Create and configure status bar connection icon label
    connectionStatusIconLabel_ = new QLabel(this);
    connectionStatusIconLabel_->setFixedWidth(20);
    connectionStatusIconLabel_->setAlignment(Qt::AlignCenter);
    ui_->statusbar->addPermanentWidget(connectionStatusIconLabel_);

    // Set up MDI area
    ui_->horizontalLayout_3->addWidget(mdiArea_);

    // Ensure viewport is initialized by accessing it
    if (mdiArea_->viewport()) {
        BOOST_LOG_SEV(lg(), debug) << "MDI area viewport initialized successfully";
    } else {
        BOOST_LOG_SEV(lg(), error) << "MDI area viewport is null!";
    }

    mdiArea_->setBackgroundLogo("ore-studio-background.png");

    // Initialize connection status icons
    const QColor iconColor(220, 220, 220); // Light gray for dark theme
    connectedIcon_ = createRecoloredIcon("ic_fluent_plug_connected_20_filled.svg", iconColor);
    disconnectedIcon_ = createRecoloredIcon("ic_fluent_plug_disconnected_20_filled.svg", iconColor);

    // Apply recolored icons for dark theme visibility (light gray color)
    ui_->ActionConnect->setIcon(createRecoloredIcon(
        "ic_fluent_plug_connected_20_filled.svg", iconColor));
    ui_->ActionDisconnect->setIcon(createRecoloredIcon(
        "ic_fluent_plug_disconnected_20_filled.svg", iconColor));
    ui_->CurrenciesAction->setIcon(createRecoloredIcon(
        "ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));
    ui_->ActionSave->setIcon(createRecoloredIcon(
        "ic_fluent_save_20_filled.svg", iconColor));
    ui_->ActionEdit->setIcon(createRecoloredIcon(
        "ic_fluent_edit_20_filled.svg", iconColor));
    ui_->ActionDelete->setIcon(createRecoloredIcon(
        "ic_fluent_delete_20_filled.svg", iconColor));
    ui_->ActionExportCSV->setIcon(createRecoloredIcon(
        "ic_fluent_document_table_20_regular.svg", iconColor));
    ui_->ActionExportXML->setIcon(createRecoloredIcon(
        "ic_fluent_document_code_16_regular.svg", iconColor));
    ui_->ActionAbout->setIcon(createRecoloredIcon(
        "ic_fluent_star_20_regular.svg", iconColor));
    ui_->ActionHistory->setIcon(createRecoloredIcon(
        "ic_fluent_history_20_regular.svg", iconColor));

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this, &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this, &MainWindow::onDisconnectTriggered);
    connect(ui_->ActionExportCSV, &QAction::triggered, this, &MainWindow::onExportCSVTriggered);
    connect(ui_->ActionExportXML, &QAction::triggered, this, &MainWindow::onExportXMLTriggered);
    connect(ui_->ActionAbout, &QAction::triggered, this, &MainWindow::onAboutTriggered);

    // Connect Window menu actions
    connect(ui_->ActionDetachAll, &QAction::triggered, this, &MainWindow::onDetachAllTriggered);
    connect(ui_->ActionReattachAll, &QAction::triggered, this, &MainWindow::onReattachAllTriggered);
    connect(ui_->menuWindow, &QMenu::aboutToShow, this, &MainWindow::onWindowMenuAboutToShow);

    // Connect CRUD actions
    connect(ui_->ActionSave, &QAction::triggered, this, [this]() {
        // Get the currently active MDI subwindow
        auto* activeWindow = mdiArea_->activeSubWindow();
        if (!activeWindow) {
            return;
        }

        // Check if it's a DetachableMdiSubWindow with a CurrencyDetailPanel
        auto* detachableWindow = qobject_cast<DetachableMdiSubWindow*>(activeWindow);
        if (detachableWindow) {
            auto* detailPanel = qobject_cast<CurrencyDetailPanel*>(detachableWindow->widget());
            if (detailPanel) {
                detailPanel->save();
            }
        }
    });
    connect(ui_->ActionEdit, &QAction::triggered, this, &MainWindow::onEditTriggered);
    connect(ui_->ActionDelete, &QAction::triggered, this, &MainWindow::onDeleteTriggered);
    connect(ui_->ActionHistory, &QAction::triggered, this, &MainWindow::onHistoryTriggered);

    // Connect to MDI area window activation to manage context-aware actions
    connect(mdiArea_, &QMdiArea::subWindowActivated,
            this, &MainWindow::onSubWindowActivated);

    // Currencies action creates/reuses MDI window with currency table
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [=, this]() {
        using ores::utility::log::warn;
        using ores::utility::log::info;

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), warn) << "Currencies action triggered but not connected";
            MessageBoxHelper::warning(this, "Not Connected",
                "Please login first to view currencies.");
            return;
        }

        // Reuse existing window if it exists
        if (currencyListWindow_) {
            BOOST_LOG_SEV(lg(), info) << "Reusing existing currencies window";

            // Bring window to front
            if (currencyListWindow_->isDetached()) {
                currencyListWindow_->show();
                currencyListWindow_->raise();
                currencyListWindow_->activateWindow();
            } else {
                mdiArea_->setActiveSubWindow(currencyListWindow_);
                currencyListWindow_->show();
                currencyListWindow_->raise();
            }

            // Refresh the data in case it changed
            auto* currencyWidget = qobject_cast<CurrencyMdiWindow*>(currencyListWindow_->widget());
            if (currencyWidget && currencyWidget->currencyModel()) {
                currencyWidget->currencyModel()->refresh();
            }
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Creating new currencies MDI window";
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
        connect(currencyWidget, &CurrencyMdiWindow::currencyDeleted,
                this, &MainWindow::onCurrencyDeleted);
        connect(currencyWidget, &CurrencyMdiWindow::showCurrencyHistory,
                this, &MainWindow::onShowCurrencyHistory);

        currencyListWindow_ = new DetachableMdiSubWindow();
        currencyListWindow_->setWidget(currencyWidget);
        currencyListWindow_->setWindowTitle("Currencies");
        currencyListWindow_->setWindowIcon(createRecoloredIcon(
            "ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

        // Track window for detach/reattach operations
        allDetachableWindows_.append(currencyListWindow_);
        connect(currencyListWindow_, &QObject::destroyed, this, [this]() {
            allDetachableWindows_.removeAll(currencyListWindow_);
            currencyListWindow_ = nullptr;
        });

        mdiArea_->addSubWindow(currencyListWindow_);

        // Size to content, not maximized
        currencyListWindow_->adjustSize();
        currencyListWindow_->show();
    });

    // Initially disable data-related actions until logged in
    updateMenuState();
    // Also disable export buttons initially since no currency window is active
    updateCrudActionState();

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

        // Set client for all detail windows if they exist
        for (auto* detailWindow : currencyDetailWindows_) {
            if (detailWindow) {
                auto* detailPanel = qobject_cast<CurrencyDetailPanel*>(detailWindow->widget());
                if (detailPanel) {
                    detailPanel->setClient(client_);
                }
            }
        }

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Successfully connected to server and authenticated.";
            updateMenuState();
            ui_->statusbar->showMessage("Successfully connected and logged in to the server.");
        } else {
            BOOST_LOG_SEV(lg(), error) << "Client is not properly connected after login.";
            MessageBoxHelper::critical(this, "Connection Error",
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

    // Update connection status icon in status bar
    if (isConnected) {
        connectionStatusIconLabel_->setPixmap(connectedIcon_.pixmap(16, 16)); // Use 16x16 for status bar
    } else {
        connectionStatusIconLabel_->setPixmap(disconnectedIcon_.pixmap(16, 16));
    }

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

        // Close all detail windows if any are open
        for (auto* detailWindow : currencyDetailWindows_) {
            if (detailWindow) {
                detailWindow->close();
                // Note: The destroyed signal will clean up the map entry
            }
        }

        updateMenuState();

        BOOST_LOG_SEV(lg(), info) << "Disconnected from server";
        ui_->statusbar->showMessage("Successfully disconnected from the server.");
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
    const QColor disabledColor(50, 50, 50); // Dark gray for disabled state

    for (int size : {16, 20, 24, 32, 48, 64}) {
        // Get pixmap from original icon
        QPixmap pixmap = originalIcon.pixmap(size, size);

        // Create normal state image
        QImage normalImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < normalImage.height(); ++y) {
            for (int x = 0; x < normalImage.width(); ++x) {
                QColor pixelColor = normalImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    normalImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(normalImage), QIcon::Normal);

        // Create disabled state image
        QImage disabledImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < disabledImage.height(); ++y) {
            for (int x = 0; x < disabledImage.width(); ++x) {
                QColor pixelColor = disabledImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(disabledColor.red());
                    pixelColor.setGreen(disabledColor.green());
                    pixelColor.setBlue(disabledColor.blue());
                    disabledImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(disabledImage), QIcon::Disabled);
    }

    return recoloredIcon;
}

void MainWindow::onSubWindowActivated(QMdiSubWindow* window) {
    // Disconnect from previous active window if any
    if (activeCurrencyWindow_) {
        disconnect(activeCurrencyWindow_, &CurrencyMdiWindow::selectionChanged,
                   this, &MainWindow::onActiveWindowSelectionChanged);
        disconnect(activeCurrencyWindow_, &CurrencyMdiWindow::showCurrencyDetails,
                   this, &MainWindow::onShowCurrencyDetails);
        activeCurrencyWindow_ = nullptr;
        selectionCount_ = 0;
    }

    // Check if the new active window is a CurrencyMdiWindow
    if (window) {
        auto* currencyWindow = qobject_cast<CurrencyMdiWindow*>(window->widget());
        if (currencyWindow) {
            activeCurrencyWindow_ = currencyWindow;

            // Connect to selection changes
            connect(activeCurrencyWindow_, &CurrencyMdiWindow::selectionChanged,
                    this, &MainWindow::onActiveWindowSelectionChanged);
            // Connect to show currency details signal
            connect(activeCurrencyWindow_, &CurrencyMdiWindow::showCurrencyDetails,
                    this, &MainWindow::onShowCurrencyDetails);
        }
    }

    // Update CRUD action states
    updateCrudActionState();
}

void MainWindow::onActiveWindowSelectionChanged(int selection_count) {
    selectionCount_ = selection_count;
    updateCrudActionState();
}

void MainWindow::updateCrudActionState() {
    // Enable Edit and History only for single selection
    // Enable Delete for one or more selections
    // Enable Export buttons only when there's an active currency window
    const bool hasActiveWindow = activeCurrencyWindow_ != nullptr;

    ui_->ActionEdit->setEnabled(hasActiveWindow && selectionCount_ == 1);
    ui_->ActionDelete->setEnabled(hasActiveWindow && selectionCount_ >= 1);
    ui_->ActionHistory->setEnabled(hasActiveWindow && selectionCount_ == 1);
    ui_->ActionExportCSV->setEnabled(hasActiveWindow);
    ui_->ActionExportXML->setEnabled(hasActiveWindow);
}

void MainWindow::onEditTriggered() {
    if (activeCurrencyWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Edit action triggered, delegating to active window";
        activeCurrencyWindow_->editSelected();
    }
}

void MainWindow::onDeleteTriggered() {
    if (activeCurrencyWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Delete action triggered, delegating to active window";
        activeCurrencyWindow_->deleteSelected();
    }
}

void MainWindow::onHistoryTriggered() {
    if (activeCurrencyWindow_) {
        BOOST_LOG_SEV(lg(), info) << "History action triggered, delegating to active window";
        activeCurrencyWindow_->viewHistorySelected();
    }
}

void MainWindow::onShowCurrencyHistory(const QString& iso_code) {
    using ores::utility::log::warn;
    using ores::utility::log::info;

    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "History requested but not connected";
        MessageBoxHelper::warning(this, "Not Connected",
            "Please ensure you are still connected to view currency history.");
        return;
    }

    // Reuse existing window if it exists for this currency
    if (currencyHistoryWindows_.contains(iso_code)) {
        auto* existingWindow = currencyHistoryWindows_[iso_code];
        if (existingWindow) {
            BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                      << iso_code.toStdString();

            // Bring window to front
            if (existingWindow->isDetached()) {
                existingWindow->show();
                existingWindow->raise();
                existingWindow->activateWindow();
            } else {
                mdiArea_->setActiveSubWindow(existingWindow);
                existingWindow->show();
                existingWindow->raise();
            }
            return;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new currency history MDI window for: "
                             << iso_code.toStdString();

    const QColor iconColor(220, 220, 220); // Same color as other icons
    auto* historyWidget = new CurrencyHistoryMdiWindow(iso_code, client_, this);

    // Connect status signals to status bar
    connect(historyWidget, &CurrencyHistoryMdiWindow::statusChanged,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(historyWidget, &CurrencyHistoryMdiWindow::errorOccurred,
            this, [this](const QString& error_message) {
        ui_->statusbar->showMessage("Error loading history: " + error_message);
    });

    auto* subWindow = new DetachableMdiSubWindow();
    subWindow->setWidget(historyWidget);
    subWindow->setWindowTitle(QString("History: %1").arg(iso_code));
    subWindow->setWindowIcon(createRecoloredIcon(
        "ic_fluent_history_20_regular.svg", iconColor));

    // Track window for detach/reattach operations and by ISO code
    currencyHistoryWindows_[iso_code] = subWindow;
    allDetachableWindows_.append(subWindow);
    connect(subWindow, &QObject::destroyed, this, [this, iso_code, subWindow]() {
        allDetachableWindows_.removeAll(subWindow);
        currencyHistoryWindows_.remove(iso_code);
    });

    mdiArea_->addSubWindow(subWindow);

    // Size to content, not maximized
    subWindow->adjustSize();
    subWindow->show();
}

void MainWindow::onShowCurrencyDetails(const risk::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), info) << "Showing currency details for: " << currency.iso_code;

    const QString iso_code = QString::fromStdString(currency.iso_code);
    const QColor iconColor(220, 220, 220); // Same color as other icons

    // Reuse existing window if it exists for this currency
    if (currencyDetailWindows_.contains(iso_code)) {
        auto* existingWindow = currencyDetailWindows_[iso_code];
        if (existingWindow) {
            BOOST_LOG_SEV(lg(), info) << "Reusing existing detail window for: "
                                      << currency.iso_code;

            // Update currency data
            auto* detailPanel = qobject_cast<CurrencyDetailPanel*>(existingWindow->widget());
            if (detailPanel) {
                detailPanel->setCurrency(currency);
            }

            // Bring window to front
            if (existingWindow->isDetached()) {
                existingWindow->show();
                existingWindow->raise();
                existingWindow->activateWindow();
            } else {
                mdiArea_->setActiveSubWindow(existingWindow);
                existingWindow->show();
                existingWindow->raise();
            }
            return;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new currency detail MDI window for: "
                             << currency.iso_code;

    // Create the detail panel
    auto* detailPanel = new CurrencyDetailPanel(this);

    // Set client if we're connected
    if (client_) {
        detailPanel->setClient(client_);
    }

    // Connect signals from panel
    connect(detailPanel, &CurrencyDetailPanel::currencyUpdated,
            this, [this]() {
        if (activeCurrencyWindow_) {
            activeCurrencyWindow_->currencyModel()->refresh();
        }
        ui_->statusbar->showMessage("Currency updated successfully.");
    });
    connect(detailPanel, &CurrencyDetailPanel::currencyDeleted,
            this, [this](const QString& iso_code) {
        if (activeCurrencyWindow_) {
            activeCurrencyWindow_->currencyModel()->refresh();
        }
        ui_->statusbar->showMessage(QString("Currency '%1' deleted.").arg(iso_code));
        onCurrencyDeleted(iso_code);
    });
    connect(detailPanel, &CurrencyDetailPanel::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(detailPanel, &CurrencyDetailPanel::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(detailPanel, &CurrencyDetailPanel::isDirtyChanged,
            this, [this](bool isDirty) {
        ui_->ActionSave->setEnabled(isDirty);
    });

    // Set initial currency data
    detailPanel->setCurrency(currency);

    // Create detachable MDI window and set the panel as its widget
    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setWidget(detailPanel);
    detailWindow->setWindowTitle(QString("Currency Details: %1").arg(iso_code));
    detailWindow->setWindowIcon(createRecoloredIcon(
        "ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    // Track window for detach/reattach operations and by ISO code
    currencyDetailWindows_[iso_code] = detailWindow;
    allDetachableWindows_.append(detailWindow);
    connect(detailWindow, &QObject::destroyed, this, [this, iso_code, detailWindow]() {
        BOOST_LOG_SEV(lg(), info) << "Currency detail window destroyed for: "
                                  << iso_code.toStdString();
        allDetachableWindows_.removeAll(detailWindow);
        currencyDetailWindows_.remove(iso_code);
    });

    // Add to MDI area
    mdiArea_->addSubWindow(detailWindow);

    // Size to content, not maximized
    detailWindow->adjustSize();
    detailWindow->show();

    BOOST_LOG_SEV(lg(), info) << "Currency detail window shown";
}

void MainWindow::onCurrencyDeleted(const QString& iso_code) {
    // If a detail window exists for the deleted currency, close it
    if (currencyDetailWindows_.contains(iso_code)) {
        auto* detailWindow = currencyDetailWindows_[iso_code];
        if (detailWindow) {
            BOOST_LOG_SEV(lg(), info) << "Closing detail window because displayed currency was deleted: "
                                     << iso_code.toStdString();
            detailWindow->close();
            // Note: The destroyed signal will clean up the map entry
        }
    }
}

void MainWindow::onExportCSVTriggered() {
    if (activeCurrencyWindow_) {
        activeCurrencyWindow_->exportToCSV();
    } else {
        MessageBoxHelper::warning(this, "No Active Window",
                "Please open the currencies window first to export data.");
    }
}

void MainWindow::onExportXMLTriggered() {
    if (activeCurrencyWindow_) {
        activeCurrencyWindow_->exportToXML();
    } else {
        MessageBoxHelper::warning(this, "No Active Window",
                "Please open the currencies window first to export data.");
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
