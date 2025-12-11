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
#include "ores.qt/CurrencyController.hpp"

#include <QPointer>
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/CurrencyDetailDialog.hpp"
#include "ores.qt/CurrencyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.risk/domain/events/currency_changed_event.hpp"

namespace ores::qt {

using namespace ores::utility::log;

namespace {
    // Event type name for currency changes
    constexpr std::string_view currency_event_name =
        eventing::domain::event_traits<risk::domain::events::currency_changed_event>::name;
}

CurrencyController::CurrencyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      allDetachableWindows_(allDetachableWindows),
      currencyListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Currency controller created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CurrencyController::onNotificationReceived);

        // Subscribe to currency change events
        BOOST_LOG_SEV(lg(), info) << "Subscribing to currency change events";
        clientManager_->subscribeToEvent(std::string{currency_event_name});
    }
}

CurrencyController::~CurrencyController() {
    BOOST_LOG_SEV(lg(), debug) << "Currency controller destroyed";

    // Unsubscribe from currency change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from currency change events";
        clientManager_->unsubscribeFromEvent(std::string{currency_event_name});
    }
}

void CurrencyController::showListWindow() {
    // We allow showing window even if disconnected (it will show empty or cached data + offline status)
    // But for now let's keep the check if strictly required, or just warn.
    // The user requirement is "windows stay open".
    // Let's allow opening it, but the window itself should handle disconnected state.
    // For now, simply passing the clientManager is enough.

    // Reuse existing window if it exists
    if (currencyListWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing currencies window";

        // Bring window to front
        if (currencyListWindow_->isDetached()) {
            currencyListWindow_->setVisible(true);
            currencyListWindow_->show();
            currencyListWindow_->raise();
            currencyListWindow_->activateWindow();
        } else {
            currencyListWindow_->setVisible(true);
            mdiArea_->setActiveSubWindow(currencyListWindow_);
            currencyListWindow_->show();
            currencyListWindow_->raise();
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new currencies MDI window";
    const QColor iconColor(220, 220, 220);
    // Assuming CurrencyMdiWindow is updated to take ClientManager*
    auto* currencyWidget = new CurrencyMdiWindow(clientManager_, username_, mainWindow_);

    // Connect status signals
    connect(currencyWidget, &CurrencyMdiWindow::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(currencyWidget, &CurrencyMdiWindow::errorOccurred,
            this, [this](const QString& err_msg) {
        emit errorMessage("Error loading currencies: " + err_msg);
    });

    // Connect currency operations (add, edit, history)
    connect(currencyWidget, &CurrencyMdiWindow::addNewRequested,
            this, &CurrencyController::onAddNewRequested);
    connect(currencyWidget, &CurrencyMdiWindow::showCurrencyDetails,
            this, &CurrencyController::onShowCurrencyDetails);
    connect(currencyWidget, &CurrencyMdiWindow::showCurrencyHistory,
            this, &CurrencyController::onShowCurrencyHistory);

    currencyListWindow_ = new DetachableMdiSubWindow();
    currencyListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    currencyListWindow_->setWidget(currencyWidget);
    currencyListWindow_->setWindowTitle("Currencies");
    currencyListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    // Track window for detach/reattach operations
    allDetachableWindows_.append(currencyListWindow_);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = currencyListWindow_;
    connect(currencyListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }

        if (self->currencyListWindow_ == windowBeingDestroyed)
            self->currencyListWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(currencyListWindow_);
    currencyListWindow_->adjustSize();
    currencyListWindow_->show();
}

void CurrencyController::closeAllWindows() {
    // We no longer close windows on disconnect!
    // But we might close them if the controller itself is destroyed (e.g. app exit)
    // The base class doesn't enforce closing.
    // MainWindow calls this on disconnect? It should NOT anymore.
    // But if we do need to close them:
    if (currencyListWindow_) {
        currencyListWindow_->close();
    }
}

void CurrencyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency requested";
    const QColor iconColor(220, 220, 220);
    risk::domain::currency new_currency;

    // Assuming CurrencyDetailDialog updated to take ClientManager*
    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (clientManager_) {
        detailDialog->setClientManager(clientManager_);
        detailDialog->setUsername(username_.toStdString());
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });

    detailDialog->setCurrency(new_currency);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        detailWindow->show();  // Show first so geometry is valid
        detailWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        detailWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else
        detailWindow->show();
}

void CurrencyController::onShowCurrencyDetails(
    const risk::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), info) << "Showing currency details for: "
                             << currency.iso_code;

    const QString isoCode = QString::fromStdString(currency.iso_code);
    const QString windowKey = build_window_key("details", isoCode);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing detail window for: "
                                  << currency.iso_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new detail window for: "
                              << currency.iso_code;
    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (clientManager_) {
        detailDialog->setClientManager(clientManager_);
        detailDialog->setUsername(username_.toStdString());
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });

    detailDialog->setCurrency(currency);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Details: %1").arg(isoCode));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    // Track this detail window
    track_window(windowKey, detailWindow);

    allDetachableWindows_.append(detailWindow);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = detailWindow;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowPtr, windowKey]() {
        if (self) {
            self->allDetachableWindows_.removeAll(windowPtr.data());
            self->untrack_window(windowKey);
        }
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        detailWindow->show();  // Show first so geometry is valid
        detailWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        detailWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        detailWindow->show();
    }
}

void CurrencyController::onShowCurrencyHistory(const QString& isoCode) {
    BOOST_LOG_SEV(lg(), info) << "Showing currency history for: "
                             << isoCode.toStdString();

    const QString windowKey = build_window_key("history", isoCode);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << isoCode.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << isoCode.toStdString();
    const QColor iconColor(220, 220, 220);

    // Assuming CurrencyHistoryDialog updated to take ClientManager*
    auto* historyWidget = new CurrencyHistoryDialog(isoCode, clientManager_,
                                                     mainWindow_);

    connect(historyWidget, &CurrencyHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyWidget, &CurrencyHistoryDialog::errorOccurred,
            this, [this](const QString& error_message) {
        emit statusMessage("Error loading history: " + error_message);
    });

    historyWidget->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyWidget);
    historyWindow->setWindowTitle(QString("History: %1").arg(isoCode));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    track_window(windowKey, historyWindow);

    allDetachableWindows_.append(historyWindow);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = historyWindow;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowPtr, windowKey]() {
        if (self) {
            self->allDetachableWindows_.removeAll(windowPtr.data());
            self->untrack_window(windowKey);
        }
    });

    mdiArea_->addSubWindow(historyWindow);
    historyWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        historyWindow->show();  // Show first so geometry is valid
        historyWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        historyWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        historyWindow->show();
    }
}

void CurrencyController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp) {
    // Check if this is a currency change event
    if (eventType != QString::fromStdString(std::string{currency_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received currency change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString();

    // If the currency list window is open, mark it as stale
    if (currencyListWindow_) {
        auto* currencyWidget = qobject_cast<CurrencyMdiWindow*>(
            currencyListWindow_->widget());
        if (currencyWidget) {
            currencyWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked currency window as stale";
        }
    }
}

}
