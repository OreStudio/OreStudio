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
#include "ores.qt/CurrencyDetailPanel.hpp" // Include the header for CurrencyDetailPanel
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::utility::log;

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent), ui_(new Ui::MainWindow), mdiArea_(new MdiAreaWithBackground()),
    activeCurrencyWindow_(nullptr), hasSelection_(false), currencyDetailPanel_(nullptr) { // Initialize currencyDetailPanel_

    BOOST_LOG_SEV(lg(), info) << "Creating the main window.";
    ui_->setupUi(this);

    // Set up CurrencyDetailPanel in the dock widget
    currencyDetailPanel_ = new CurrencyDetailPanel(this); // No client in constructor
    ui_->currencyDetailDockWidget->setWidget(currencyDetailPanel_);
    ui_->currencyDetailDockWidget->setVisible(false); // Initially hidden

    // Connect signals from CurrencyDetailPanel to MainWindow
    connect(currencyDetailPanel_, &CurrencyDetailPanel::currencyUpdated,
            this, [this]() {
        if (activeCurrencyWindow_) {
            activeCurrencyWindow_->currencyModel()->refresh();
        }
        ui_->statusbar->showMessage("Currency updated successfully.");
    });
    connect(currencyDetailPanel_, &CurrencyDetailPanel::currencyDeleted,
            this, [this](const QString& iso_code) {
        if (activeCurrencyWindow_) {
            activeCurrencyWindow_->currencyModel()->refresh();
        }
        ui_->statusbar->showMessage(QString("Currency '%1' deleted.").arg(iso_code));
        onCurrencyDeleted(iso_code); // Clear panel if it's displaying the deleted currency
    });
    connect(currencyDetailPanel_, &CurrencyDetailPanel::statusMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(currencyDetailPanel_, &CurrencyDetailPanel::errorMessage,
            this, [this](const QString& message) {
        ui_->statusbar->showMessage(message);
    });
    connect(currencyDetailPanel_, &CurrencyDetailPanel::isDirtyChanged,
            this, [this](bool isDirty) {
        ui_->ActionSave->setEnabled(isDirty);
    });

    // Set application icon
    setWindowIcon(QIcon("modern-icon.png"));

    // Create and configure status bar connection icon label
    connectionStatusIconLabel_ = new QLabel(this);
    connectionStatusIconLabel_->setFixedWidth(20);
    connectionStatusIconLabel_->setAlignment(Qt::AlignCenter);
    ui_->statusbar->addPermanentWidget(connectionStatusIconLabel_);

    // Set up MDI area
    ui_->horizontalLayout_3->addWidget(mdiArea_);
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

    // Connect menu actions
    connect(ui_->ActionConnect, &QAction::triggered, this, &MainWindow::onLoginTriggered);
    connect(ui_->ActionDisconnect, &QAction::triggered, this, &MainWindow::onDisconnectTriggered);

    // Connect CRUD actions
    connect(ui_->ActionSave, &QAction::triggered, this, [this]() {
        if (currencyDetailPanel_) {
            currencyDetailPanel_->save();
        }
    });
    connect(ui_->ActionEdit, &QAction::triggered, this, &MainWindow::onEditTriggered);
    connect(ui_->ActionDelete, &QAction::triggered, this, &MainWindow::onDeleteTriggered);

    // Connect to MDI area window activation to manage context-aware actions
    connect(mdiArea_, &QMdiArea::subWindowActivated,
            this, &MainWindow::onSubWindowActivated);

    // Currencies action creates MDI window with currency table
    connect(ui_->CurrenciesAction, &QAction::triggered, this, [=, this]() {
        using ores::utility::log::warn;
        using ores::utility::log::info;

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), warn) << "Currencies action triggered but not connected";
            MessageBoxHelper::warning(this, "Not Connected",
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
        connect(currencyWidget, &CurrencyMdiWindow::currencyDeleted,
                this, &MainWindow::onCurrencyDeleted);

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

        // Set client for the detail panel
        if (currencyDetailPanel_) {
            currencyDetailPanel_->setClient(client_);
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

        // Clear client from detail panel
        if (currencyDetailPanel_) {
            currencyDetailPanel_->setClient(nullptr);
            currencyDetailPanel_->clearPanel();
            ui_->currencyDetailDockWidget->setVisible(false);
            displayedCurrencyIsoCode_.clear();
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
        activeCurrencyWindow_ = nullptr;
        hasSelection_ = false;
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

void MainWindow::onActiveWindowSelectionChanged(bool has_selection) {
    hasSelection_ = has_selection;
    updateCrudActionState();
}

void MainWindow::updateCrudActionState() {
    // Enable CRUD actions only if:
    // 1. There's an active currency window
    // 2. That window has a selection
    const bool enable = activeCurrencyWindow_ != nullptr && hasSelection_;

    ui_->ActionEdit->setEnabled(enable);
    ui_->ActionDelete->setEnabled(enable);
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

void MainWindow::onShowCurrencyDetails(const risk::domain::currency& currency) {
    if (currencyDetailPanel_) {
        displayedCurrencyIsoCode_ = QString::fromStdString(currency.iso_code);
        currencyDetailPanel_->setCurrency(currency);
        ui_->currencyDetailDockWidget->setVisible(true);
        ui_->currencyDetailDockWidget->raise(); // Bring to front
    }
}

void MainWindow::onCurrencyDeleted(const QString& iso_code) {
    // If the deleted currency is currently displayed in the panel, clear and hide it
    if (displayedCurrencyIsoCode_ == iso_code) {
        BOOST_LOG_SEV(lg(), info) << "Clearing panel because displayed currency was deleted: "
                                 << iso_code.toStdString();
        if (currencyDetailPanel_) {
            currencyDetailPanel_->clearPanel();
            ui_->currencyDetailDockWidget->setVisible(false);
        }
        displayedCurrencyIsoCode_.clear();
    }
}

}
