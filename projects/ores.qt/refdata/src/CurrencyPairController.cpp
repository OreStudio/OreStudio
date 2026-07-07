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
#include "ores.qt/CurrencyPairController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CurrencyPairDetailDialog.hpp"
#include "ores.qt/CurrencyPairHistoryDialog.hpp"
#include "ores.qt/CurrencyPairMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/currency_pair_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view pair_event_name =
    eventing::domain::event_traits<refdata::eventing::currency_pair_changed_event>::name;
}

CurrencyPairController::CurrencyPairController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               ImageCache* imageCache,
                                               ChangeReasonCache* changeReasonCache,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, pair_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "CurrencyPairController created";
}

void CurrencyPairController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "currency_pairs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CurrencyPairMdiWindow(clientManager_, username_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &CurrencyPairMdiWindow::statusChanged,
            this,
            &CurrencyPairController::statusMessage);
    connect(listWindow_,
            &CurrencyPairMdiWindow::errorOccurred,
            this,
            &CurrencyPairController::errorMessage);
    connect(listWindow_,
            &CurrencyPairMdiWindow::showPairDetails,
            this,
            &CurrencyPairController::onShowDetails);
    connect(listWindow_,
            &CurrencyPairMdiWindow::addNewRequested,
            this,
            &CurrencyPairController::onAddNewRequested);
    connect(listWindow_,
            &CurrencyPairMdiWindow::showPairHistory,
            this,
            &CurrencyPairController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Currency Pairs");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));
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
            [self = QPointer<CurrencyPairController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Currency Pair list window created";
}

void CurrencyPairController::closeAllWindows() {
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

void CurrencyPairController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CurrencyPairController::onShowDetails(const refdata::domain::currency_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << pair.pair_code;
    showDetailWindow(pair);
}

void CurrencyPairController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency pair requested";
    showAddWindow();
}

void CurrencyPairController::onShowHistory(const refdata::domain::currency_pair& pair) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << pair.pair_code;
    showHistoryWindow(QString::fromStdString(pair.pair_code));
}

void CurrencyPairController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new currency pair";

    auto* detailDialog = new CurrencyPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CurrencyPairDetailDialog::statusMessage,
            this,
            &CurrencyPairController::statusMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::errorMessage,
            this,
            &CurrencyPairController::errorMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency Pair");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyPairController::showDetailWindow(const refdata::domain::currency_pair& pair) {

    const QString identifier = QString::fromStdString(pair.pair_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << pair.pair_code;

    auto* detailDialog = new CurrencyPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setPair(pair);

    connect(detailDialog,
            &CurrencyPairDetailDialog::statusMessage,
            this,
            &CurrencyPairController::statusMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::errorMessage,
            this,
            &CurrencyPairController::errorMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Pair saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CurrencyPairDetailDialog::pairDeleted,
            this,
            [self = QPointer<CurrencyPairController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Pair deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Pair: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CurrencyPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyPairController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for currency pair: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new CurrencyPairHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CurrencyPairHistoryDialog::statusChanged,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CurrencyPairHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CurrencyPairHistoryDialog::revertVersionRequested,
            this,
            &CurrencyPairController::onRevertVersion);
    connect(historyDialog,
            &CurrencyPairHistoryDialog::openVersionRequested,
            this,
            &CurrencyPairController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Currency Pair History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CurrencyPairController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CurrencyPairController::onOpenVersion(const refdata::domain::currency_pair& pair,
                                           int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency pair: " << pair.pair_code;

    const QString code = QString::fromStdString(pair.pair_code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CurrencyPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPair(pair);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CurrencyPairDetailDialog::statusMessage,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CurrencyPairDetailDialog::errorMessage,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Currency Pair: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyPairController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CurrencyPairController::onRevertVersion(const refdata::domain::currency_pair& pair) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency pair to version: " << pair.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CurrencyPairDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_pair = pair;
    reverted_pair.version = 0;
    detailDialog->setPair(reverted_pair);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CurrencyPairDetailDialog::statusMessage,
            this,
            &CurrencyPairController::statusMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::errorMessage,
            this,
            &CurrencyPairController::errorMessage);
    connect(detailDialog,
            &CurrencyPairDetailDialog::pairSaved,
            this,
            [self = QPointer<CurrencyPairController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency Pair reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Currency Pair '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Currency Pair: %1").arg(QString::fromStdString(pair.pair_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CurrencyPairController::listWindow() const {
    return listWindow_;
}

}
