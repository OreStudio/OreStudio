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
#include "ores.qt/TradeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TradeMdiWindow.hpp"
#include "ores.qt/TradeDetailDialog.hpp"
#include "ores.qt/TradeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.trade/eventing/trade_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view trade_event_name =
    eventing::domain::event_traits<trade::eventing::trade_changed_event>::name;
}

TradeController::TradeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          trade_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TradeController created";
}

void TradeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "trades");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TradeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &TradeMdiWindow::statusChanged,
            this, &TradeController::statusMessage);
    connect(listWindow_, &TradeMdiWindow::errorOccurred,
            this, &TradeController::errorMessage);
    connect(listWindow_, &TradeMdiWindow::showTradeDetails,
            this, &TradeController::onShowDetails);
    connect(listWindow_, &TradeMdiWindow::addNewRequested,
            this, &TradeController::onAddNewRequested);
    connect(listWindow_, &TradeMdiWindow::showTradeHistory,
            this, &TradeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Trades");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<TradeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Trade list window created";
}

void TradeController::closeAllWindows() {
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

void TradeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TradeController::onShowDetails(
    const trade::domain::trade& trade) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << trade.external_id;
    showDetailWindow(trade);
}

void TradeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new trade requested";
    showAddWindow();
}

void TradeController::onShowHistory(
    const trade::domain::trade& trade) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << trade.external_id;
    showHistoryWindow(trade);
}

void TradeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new trade";

    auto* detailDialog = new TradeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &TradeDetailDialog::statusMessage,
            this, &TradeController::statusMessage);
    connect(detailDialog, &TradeDetailDialog::errorMessage,
            this, &TradeController::errorMessage);
    connect(detailDialog, &TradeDetailDialog::tradeSaved,
            this, [self = QPointer<TradeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Trade saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Trade");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TradeController::showDetailWindow(
    const trade::domain::trade& trade) {

    const QString identifier = QString::fromStdString(trade.external_id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << trade.external_id;

    auto* detailDialog = new TradeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setTrade(trade);

    connect(detailDialog, &TradeDetailDialog::statusMessage,
            this, &TradeController::statusMessage);
    connect(detailDialog, &TradeDetailDialog::errorMessage,
            this, &TradeController::errorMessage);
    connect(detailDialog, &TradeDetailDialog::tradeSaved,
            this, [self = QPointer<TradeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Trade saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &TradeDetailDialog::tradeDeleted,
            this, [self = QPointer<TradeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Trade deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Trade: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TradeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TradeController::showHistoryWindow(
    const trade::domain::trade& trade) {
    const QString code = QString::fromStdString(trade.external_id);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for trade: "
                              << trade.external_id;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << trade.external_id;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << trade.external_id;

    auto* historyDialog = new TradeHistoryDialog(
        trade.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &TradeHistoryDialog::statusChanged,
            this, [self = QPointer<TradeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &TradeHistoryDialog::errorOccurred,
            this, [self = QPointer<TradeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &TradeHistoryDialog::revertVersionRequested,
            this, &TradeController::onRevertVersion);
    connect(historyDialog, &TradeHistoryDialog::openVersionRequested,
            this, &TradeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Trade History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<TradeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TradeController::onOpenVersion(
    const trade::domain::trade& trade, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for trade: " << trade.external_id;

    const QString code = QString::fromStdString(trade.external_id);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TradeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setTrade(trade);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &TradeDetailDialog::statusMessage,
            this, [self = QPointer<TradeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &TradeDetailDialog::errorMessage,
            this, [self = QPointer<TradeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Trade: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TradeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TradeController::onRevertVersion(
    const trade::domain::trade& trade) {
    BOOST_LOG_SEV(lg(), info) << "Reverting trade to version: "
                              << trade.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TradeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setTrade(trade);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &TradeDetailDialog::statusMessage,
            this, &TradeController::statusMessage);
    connect(detailDialog, &TradeDetailDialog::errorMessage,
            this, &TradeController::errorMessage);
    connect(detailDialog, &TradeDetailDialog::tradeSaved,
            this, [self = QPointer<TradeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Trade reverted: " << code.toStdString();
        emit self->statusMessage(QString("Trade '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Trade: %1")
        .arg(QString::fromStdString(trade.external_id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TradeController::listWindow() const {
    return listWindow_;
}

}
