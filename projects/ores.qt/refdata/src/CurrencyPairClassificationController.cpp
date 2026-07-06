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
#include "ores.qt/CurrencyPairClassificationController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CurrencyPairClassificationDetailDialog.hpp"
#include "ores.qt/CurrencyPairClassificationHistoryDialog.hpp"
#include "ores.qt/CurrencyPairClassificationMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/currency_pair_classification_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view classification_event_name = eventing::domain::event_traits<
    refdata::eventing::currency_pair_classification_changed_event>::name;
}

CurrencyPairClassificationController::CurrencyPairClassificationController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, classification_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CurrencyPairClassificationController created";
}

void CurrencyPairClassificationController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "currency_pair_classifications");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CurrencyPairClassificationMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CurrencyPairClassificationMdiWindow::statusChanged,
            this,
            &CurrencyPairClassificationController::statusMessage);
    connect(listWindow_,
            &CurrencyPairClassificationMdiWindow::errorOccurred,
            this,
            &CurrencyPairClassificationController::errorMessage);
    connect(listWindow_,
            &CurrencyPairClassificationMdiWindow::showClassificationDetails,
            this,
            &CurrencyPairClassificationController::onShowDetails);
    connect(listWindow_,
            &CurrencyPairClassificationMdiWindow::addNewRequested,
            this,
            &CurrencyPairClassificationController::onAddNewRequested);
    connect(listWindow_,
            &CurrencyPairClassificationMdiWindow::showClassificationHistory,
            this,
            &CurrencyPairClassificationController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Currency Pair Classifications");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Currency Pair Classification list window created";
}

void CurrencyPairClassificationController::closeAllWindows() {
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

void CurrencyPairClassificationController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CurrencyPairClassificationController::onShowDetails(
    const refdata::domain::currency_pair_classification& classification) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << classification.code;
    showDetailWindow(classification);
}

void CurrencyPairClassificationController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency pair classification requested";
    showAddWindow();
}

void CurrencyPairClassificationController::onShowHistory(
    const refdata::domain::currency_pair_classification& classification) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << classification.code;
    showHistoryWindow(QString::fromStdString(classification.code));
}

void CurrencyPairClassificationController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new currency pair classification";

    auto* detailDialog = new CurrencyPairClassificationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::statusMessage,
            this,
            &CurrencyPairClassificationController::statusMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::errorMessage,
            this,
            &CurrencyPairClassificationController::errorMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::classificationSaved,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Currency Pair Classification saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency Pair Classification");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyPairClassificationController::showDetailWindow(
    const refdata::domain::currency_pair_classification& classification) {

    const QString identifier = QString::fromStdString(classification.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << classification.code;

    auto* detailDialog = new CurrencyPairClassificationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setClassification(classification);

    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::statusMessage,
            this,
            &CurrencyPairClassificationController::statusMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::errorMessage,
            this,
            &CurrencyPairClassificationController::errorMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::classificationSaved,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Currency Pair Classification saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(
        detailDialog,
        &CurrencyPairClassificationDetailDialog::classificationDeleted,
        this,
        [self = QPointer<CurrencyPairClassificationController>(this), key](const QString& code) {
            if (!self)
                return;
            BOOST_LOG_SEV(lg(), info)
                << "Currency Pair Classification deleted: " << code.toStdString();
            self->handleEntityDeleted();
        });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Pair Classification: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CurrencyPairClassificationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyPairClassificationController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for currency pair classification: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new CurrencyPairClassificationHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CurrencyPairClassificationHistoryDialog::statusChanged,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CurrencyPairClassificationHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CurrencyPairClassificationHistoryDialog::revertVersionRequested,
            this,
            &CurrencyPairClassificationController::onRevertVersion);
    connect(historyDialog,
            &CurrencyPairClassificationHistoryDialog::openVersionRequested,
            this,
            &CurrencyPairClassificationController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Currency Pair Classification History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CurrencyPairClassificationController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CurrencyPairClassificationController::onOpenVersion(
    const refdata::domain::currency_pair_classification& classification, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency pair classification: " << classification.code;

    const QString code = QString::fromStdString(classification.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CurrencyPairClassificationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setClassification(classification);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::statusMessage,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::errorMessage,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Currency Pair Classification: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyPairClassificationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CurrencyPairClassificationController::onRevertVersion(
    const refdata::domain::currency_pair_classification& classification) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency pair classification to version: "
                              << classification.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CurrencyPairClassificationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_classification = classification;
    reverted_classification.version = 0;
    detailDialog->setClassification(reverted_classification);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::statusMessage,
            this,
            &CurrencyPairClassificationController::statusMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::errorMessage,
            this,
            &CurrencyPairClassificationController::errorMessage);
    connect(detailDialog,
            &CurrencyPairClassificationDetailDialog::classificationSaved,
            this,
            [self = QPointer<CurrencyPairClassificationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Currency Pair Classification reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Currency Pair Classification '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Currency Pair Classification: %1")
                                     .arg(QString::fromStdString(classification.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CurrencyPairClassificationController::listWindow() const {
    return listWindow_;
}

}
