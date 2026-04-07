/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/PricingModelConfigController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PricingModelConfigMdiWindow.hpp"
#include "ores.qt/PricingModelConfigDetailDialog.hpp"
#include "ores.qt/PricingModelConfigHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelConfigController::PricingModelConfigController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PricingModelConfigController created";
}

void PricingModelConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "pricing_model_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PricingModelConfigMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PricingModelConfigMdiWindow::statusChanged,
            this, &PricingModelConfigController::statusMessage);
    connect(listWindow_, &PricingModelConfigMdiWindow::errorOccurred,
            this, &PricingModelConfigController::errorMessage);
    connect(listWindow_, &PricingModelConfigMdiWindow::showConfigDetails,
            this, &PricingModelConfigController::onShowDetails);
    connect(listWindow_, &PricingModelConfigMdiWindow::addNewRequested,
            this, &PricingModelConfigController::onAddNewRequested);
    connect(listWindow_, &PricingModelConfigMdiWindow::showConfigHistory,
            this, &PricingModelConfigController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Pricing Model Configurations");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PricingModelConfigController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Pricing Model Configuration list window created";
}

void PricingModelConfigController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void PricingModelConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PricingModelConfigController::onShowDetails(
    const analytics::domain::pricing_model_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << config.name;
    showDetailWindow(config);
}

void PricingModelConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new pricing model configuration requested";
    showAddWindow();
}

void PricingModelConfigController::onShowHistory(
    const analytics::domain::pricing_model_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << config.name;
    showHistoryWindow(config);
}

void PricingModelConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new pricing model configuration";

    auto* detailDialog = new PricingModelConfigDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PricingModelConfigDetailDialog::statusMessage,
            this, &PricingModelConfigController::statusMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::errorMessage,
            this, &PricingModelConfigController::errorMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::configSaved,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Pricing Model Configuration");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelConfigController::showDetailWindow(
    const analytics::domain::pricing_model_config& config) {

    const QString identifier = QString::fromStdString(config.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << config.name;

    auto* detailDialog = new PricingModelConfigDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(config);

    connect(detailDialog, &PricingModelConfigDetailDialog::statusMessage,
            this, &PricingModelConfigController::statusMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::errorMessage,
            this, &PricingModelConfigController::errorMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::configSaved,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PricingModelConfigDetailDialog::configDeleted,
            this, [self = QPointer<PricingModelConfigController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Configuration: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelConfigController::showHistoryWindow(
    const analytics::domain::pricing_model_config& config) {
    const QString code = QString::fromStdString(config.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for pricing model configuration: "
                              << config.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << config.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << config.name;

    auto* historyDialog = new PricingModelConfigHistoryDialog(
        config.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &PricingModelConfigHistoryDialog::statusChanged,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PricingModelConfigHistoryDialog::errorOccurred,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PricingModelConfigHistoryDialog::revertVersionRequested,
            this, &PricingModelConfigController::onRevertVersion);
    connect(historyDialog, &PricingModelConfigHistoryDialog::openVersionRequested,
            this, &PricingModelConfigController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Pricing Model Configuration History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PricingModelConfigController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PricingModelConfigController::onOpenVersion(
    const analytics::domain::pricing_model_config& config, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for pricing model configuration: " << config.name;

    const QString code = QString::fromStdString(config.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PricingModelConfigDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(config);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PricingModelConfigDetailDialog::statusMessage,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PricingModelConfigDetailDialog::errorMessage,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Configuration: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PricingModelConfigController::onRevertVersion(
    const analytics::domain::pricing_model_config& config) {
    BOOST_LOG_SEV(lg(), info) << "Reverting pricing model configuration to version: "
                              << config.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PricingModelConfigDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConfig(config);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PricingModelConfigDetailDialog::statusMessage,
            this, &PricingModelConfigController::statusMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::errorMessage,
            this, &PricingModelConfigController::errorMessage);
    connect(detailDialog, &PricingModelConfigDetailDialog::configSaved,
            this, [self = QPointer<PricingModelConfigController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration reverted: " << code.toStdString();
        emit self->statusMessage(QString("Pricing Model Configuration '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Pricing Model Configuration: %1")
        .arg(QString::fromStdString(config.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PricingModelConfigController::listWindow() const {
    return listWindow_;
}

}
