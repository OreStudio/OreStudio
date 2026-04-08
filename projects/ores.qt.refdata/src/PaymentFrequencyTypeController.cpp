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
#include "ores.qt/PaymentFrequencyTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PaymentFrequencyTypeMdiWindow.hpp"
#include "ores.qt/PaymentFrequencyTypeDetailDialog.hpp"
#include "ores.qt/PaymentFrequencyTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PaymentFrequencyTypeController::PaymentFrequencyTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PaymentFrequencyTypeController created";
}

void PaymentFrequencyTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "payment_frequency_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PaymentFrequencyTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PaymentFrequencyTypeMdiWindow::statusChanged,
            this, &PaymentFrequencyTypeController::statusMessage);
    connect(listWindow_, &PaymentFrequencyTypeMdiWindow::errorOccurred,
            this, &PaymentFrequencyTypeController::errorMessage);
    connect(listWindow_, &PaymentFrequencyTypeMdiWindow::showTypeDetails,
            this, &PaymentFrequencyTypeController::onShowDetails);
    connect(listWindow_, &PaymentFrequencyTypeMdiWindow::addNewRequested,
            this, &PaymentFrequencyTypeController::onAddNewRequested);
    connect(listWindow_, &PaymentFrequencyTypeMdiWindow::showTypeHistory,
            this, &PaymentFrequencyTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Payment Frequency Types");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Clock, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PaymentFrequencyTypeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Payment Frequency Type list window created";
}

void PaymentFrequencyTypeController::closeAllWindows() {
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

void PaymentFrequencyTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PaymentFrequencyTypeController::onShowDetails(
    const trading::domain::payment_frequency_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void PaymentFrequencyTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new payment frequency type requested";
    showAddWindow();
}

void PaymentFrequencyTypeController::onShowHistory(
    const trading::domain::payment_frequency_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void PaymentFrequencyTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new payment frequency type";

    auto* detailDialog = new PaymentFrequencyTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::statusMessage,
            this, &PaymentFrequencyTypeController::statusMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::errorMessage,
            this, &PaymentFrequencyTypeController::errorMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::typeSaved,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Payment Frequency Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Payment Frequency Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Clock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PaymentFrequencyTypeController::showDetailWindow(
    const trading::domain::payment_frequency_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new PaymentFrequencyTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::statusMessage,
            this, &PaymentFrequencyTypeController::statusMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::errorMessage,
            this, &PaymentFrequencyTypeController::errorMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::typeSaved,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Payment Frequency Type saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::typeDeleted,
            this, [self = QPointer<PaymentFrequencyTypeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Payment Frequency Type deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Payment Frequency Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Clock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PaymentFrequencyTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PaymentFrequencyTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for payment frequency type: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();

    auto* historyDialog = new PaymentFrequencyTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &PaymentFrequencyTypeHistoryDialog::statusChanged,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PaymentFrequencyTypeHistoryDialog::errorOccurred,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PaymentFrequencyTypeHistoryDialog::revertVersionRequested,
            this, &PaymentFrequencyTypeController::onRevertVersion);
    connect(historyDialog, &PaymentFrequencyTypeHistoryDialog::openVersionRequested,
            this, &PaymentFrequencyTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Payment Frequency Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PaymentFrequencyTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PaymentFrequencyTypeController::onOpenVersion(
    const trading::domain::payment_frequency_type& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for payment frequency type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PaymentFrequencyTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::statusMessage,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::errorMessage,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Payment Frequency Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PaymentFrequencyTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PaymentFrequencyTypeController::onRevertVersion(
    const trading::domain::payment_frequency_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting payment frequency type to version: "
                              << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PaymentFrequencyTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::statusMessage,
            this, &PaymentFrequencyTypeController::statusMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::errorMessage,
            this, &PaymentFrequencyTypeController::errorMessage);
    connect(detailDialog, &PaymentFrequencyTypeDetailDialog::typeSaved,
            this, [self = QPointer<PaymentFrequencyTypeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Payment Frequency Type reverted: " << code.toStdString();
        emit self->statusMessage(QString("Payment Frequency Type '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Payment Frequency Type: %1")
        .arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PaymentFrequencyTypeController::listWindow() const {
    return listWindow_;
}

}
