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
#include "ores.qt/PricingModelProductParameterController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PricingModelProductParameterMdiWindow.hpp"
#include "ores.qt/PricingModelProductParameterDetailDialog.hpp"
#include "ores.qt/PricingModelProductParameterHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelProductParameterController::PricingModelProductParameterController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PricingModelProductParameterController created";
}

void PricingModelProductParameterController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "pricing_model_product_parameters");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PricingModelProductParameterMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PricingModelProductParameterMdiWindow::statusChanged,
            this, &PricingModelProductParameterController::statusMessage);
    connect(listWindow_, &PricingModelProductParameterMdiWindow::errorOccurred,
            this, &PricingModelProductParameterController::errorMessage);
    connect(listWindow_, &PricingModelProductParameterMdiWindow::showParameterDetails,
            this, &PricingModelProductParameterController::onShowDetails);
    connect(listWindow_, &PricingModelProductParameterMdiWindow::addNewRequested,
            this, &PricingModelProductParameterController::onAddNewRequested);
    connect(listWindow_, &PricingModelProductParameterMdiWindow::showParameterHistory,
            this, &PricingModelProductParameterController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Pricing Model Product Parameters");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PricingModelProductParameterController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Pricing Model Product Parameter list window created";
}

void PricingModelProductParameterController::closeAllWindows() {
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

void PricingModelProductParameterController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PricingModelProductParameterController::onShowDetails(
    const analytics::domain::pricing_model_product_parameter& parameter) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << parameter.parameter_name;
    showDetailWindow(parameter);
}

void PricingModelProductParameterController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new pricing model product parameter requested";
    showAddWindow();
}

void PricingModelProductParameterController::onShowHistory(
    const analytics::domain::pricing_model_product_parameter& parameter) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << parameter.parameter_name;
    showHistoryWindow(parameter);
}

void PricingModelProductParameterController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new pricing model product parameter";

    auto* detailDialog = new PricingModelProductParameterDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PricingModelProductParameterDetailDialog::statusMessage,
            this, &PricingModelProductParameterController::statusMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::errorMessage,
            this, &PricingModelProductParameterController::errorMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::parameterSaved,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Pricing Model Product Parameter");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelProductParameterController::showDetailWindow(
    const analytics::domain::pricing_model_product_parameter& parameter) {

    const QString identifier = QString::fromStdString(parameter.parameter_name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << parameter.parameter_name;

    auto* detailDialog = new PricingModelProductParameterDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setParameter(parameter);

    connect(detailDialog, &PricingModelProductParameterDetailDialog::statusMessage,
            this, &PricingModelProductParameterController::statusMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::errorMessage,
            this, &PricingModelProductParameterController::errorMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::parameterSaved,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PricingModelProductParameterDetailDialog::parameterDeleted,
            this, [self = QPointer<PricingModelProductParameterController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Product Parameter: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Settings, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelProductParameterController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelProductParameterController::showHistoryWindow(
    const analytics::domain::pricing_model_product_parameter& parameter) {
    const QString code = QString::fromStdString(parameter.parameter_name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for pricing model product parameter: "
                              << parameter.parameter_name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << parameter.parameter_name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << parameter.parameter_name;

    auto* historyDialog = new PricingModelProductParameterHistoryDialog(
        parameter.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &PricingModelProductParameterHistoryDialog::statusChanged,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PricingModelProductParameterHistoryDialog::errorOccurred,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PricingModelProductParameterHistoryDialog::revertVersionRequested,
            this, &PricingModelProductParameterController::onRevertVersion);
    connect(historyDialog, &PricingModelProductParameterHistoryDialog::openVersionRequested,
            this, &PricingModelProductParameterController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Pricing Model Product Parameter History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PricingModelProductParameterController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PricingModelProductParameterController::onOpenVersion(
    const analytics::domain::pricing_model_product_parameter& parameter, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for pricing model product parameter: " << parameter.parameter_name;

    const QString code = QString::fromStdString(parameter.parameter_name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PricingModelProductParameterDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setParameter(parameter);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PricingModelProductParameterDetailDialog::statusMessage,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PricingModelProductParameterDetailDialog::errorMessage,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Product Parameter: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelProductParameterController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PricingModelProductParameterController::onRevertVersion(
    const analytics::domain::pricing_model_product_parameter& parameter) {
    BOOST_LOG_SEV(lg(), info) << "Reverting pricing model product parameter to version: "
                              << parameter.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PricingModelProductParameterDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setParameter(parameter);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PricingModelProductParameterDetailDialog::statusMessage,
            this, &PricingModelProductParameterController::statusMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::errorMessage,
            this, &PricingModelProductParameterController::errorMessage);
    connect(detailDialog, &PricingModelProductParameterDetailDialog::parameterSaved,
            this, [self = QPointer<PricingModelProductParameterController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter reverted: " << code.toStdString();
        emit self->statusMessage(QString("Pricing Model Product Parameter '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Pricing Model Product Parameter: %1")
        .arg(QString::fromStdString(parameter.parameter_name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PricingModelProductParameterController::listWindow() const {
    return listWindow_;
}

}
