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
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CurrencyDetailDialog.hpp"
#include "ores.qt/CurrencyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.risk/eventing/currency_changed_event.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

namespace {
    // Event type name for currency changes
    constexpr std::string_view currency_event_name =
        eventing::domain::event_traits<risk::eventing::currency_changed_event>::name;
}

CurrencyController::CurrencyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      allDetachableWindows_(allDetachableWindows),
      imageCache_(imageCache),
      changeReasonCache_(changeReasonCache),
      currencyListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Currency controller created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CurrencyController::onNotificationReceived);

        // Subscribe to events when connected (event adapter only available after login)
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to currency change events";
            clientManager_->subscribeToEvent(std::string{currency_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to currency change events after reconnect";
            clientManager_->subscribeToEvent(std::string{currency_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to currency change events";
            clientManager_->subscribeToEvent(std::string{currency_event_name});
        }
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
    auto* currencyWidget = new CurrencyMdiWindow(clientManager_, imageCache_,
                                                  username_, mainWindow_);

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
        ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg", iconColor));

    // Track window for detach/reattach operations
    allDetachableWindows_.append(currencyListWindow_);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = currencyListWindow_;
    // to avoid race conditions with signal processing
    connect(currencyListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed](QObject* obj) {
            if (!self) return;

            BOOST_LOG_SEV(lg(), debug) << "Detachable MDI Sub Window destroyed";

            // Remove the window from the list of all detachable windows
            self->allDetachableWindows_.removeAll(windowBeingDestroyed);

            // If the destroyed window is the currency list, nullify the pointer
            if (self->currencyListWindow_ == windowBeingDestroyed)
                self->currencyListWindow_ = nullptr;
        });
    
    // When the image cache reloads (due to external event), mark the list as stale
    connect(imageCache_, &ImageCache::allLoaded, this, [this, currencyWidget](){
        if (currencyWidget) {
            currencyWidget->markAsStale();
        }
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
    if (imageCache_) {
        detailDialog->setImageCache(imageCache_);
    }
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    detailDialog->setCurrency(new_currency);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    mdiArea_->addSubWindow(detailWindow);
    // Set window flags AFTER addSubWindow (Qt resets flags when adding to MDI)
    detailWindow->setWindowFlags(detailWindow->windowFlags()
        & ~Qt::WindowMaximizeButtonHint);
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
    if (imageCache_) {
        detailDialog->setImageCache(imageCache_);
    }
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    detailDialog->setCurrency(currency);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Details: %1").arg(isoCode));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg", iconColor));

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
    // Set window flags AFTER addSubWindow (Qt resets flags when adding to MDI)
    detailWindow->setWindowFlags(detailWindow->windowFlags()
        & ~Qt::WindowMaximizeButtonHint);
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

    auto* historyWidget = new CurrencyHistoryDialog(isoCode, clientManager_,
                                                     mainWindow_);
    if (imageCache_) {
        historyWidget->setImageCache(imageCache_);
    }

    connect(historyWidget, &CurrencyHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyWidget, &CurrencyHistoryDialog::errorOccurred,
            this, [this](const QString& error_message) {
        emit statusMessage("Error loading history: " + error_message);
    });

    // Connect open and revert signals
    connect(historyWidget, &CurrencyHistoryDialog::openVersionRequested,
            this, &CurrencyController::onOpenCurrencyVersion);
    connect(historyWidget, &CurrencyHistoryDialog::revertVersionRequested,
            this, &CurrencyController::onRevertCurrency);

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
    // Set window flags AFTER addSubWindow (Qt resets flags when adding to MDI)
    historyWindow->setWindowFlags(historyWindow->windowFlags()
        & ~Qt::WindowMaximizeButtonHint);
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
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {
    // Check if this is a currency change event
    if (eventType != QString::fromStdString(std::string{currency_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received currency change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " ISO codes";

    // If the currency list window is open, mark it as stale
    if (currencyListWindow_) {
        auto* currencyWidget = qobject_cast<CurrencyMdiWindow*>(
            currencyListWindow_->widget());
        if (currencyWidget) {
            currencyWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked currency window as stale";
        }
    }

    // Notify open detail/history dialogs for affected currencies
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        const QString& key = it.key();
        auto* window = it.value();
        if (!window)
            continue;

        if (key.startsWith("details:")) {
            if (auto* detailDialog = qobject_cast<CurrencyDetailDialog*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(detailDialog->isoCode())) {
                    detailDialog->markAsStale();
                    BOOST_LOG_SEV(lg(), debug) << "Marked detail dialog as stale for: "
                                               << detailDialog->isoCode().toStdString();
                }
            }
        } else if (key.startsWith("history:")) {
            if (auto* historyDialog = qobject_cast<CurrencyHistoryDialog*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(historyDialog->isoCode())) {
                    historyDialog->markAsStale();
                    BOOST_LOG_SEV(lg(), debug) << "Marked history dialog as stale for: "
                                               << historyDialog->isoCode().toStdString();
                }
            }
        }
    }
}

void CurrencyController::onOpenCurrencyVersion(
    const risk::domain::currency& currency, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency: " << currency.iso_code;

    const QString isoCode = QString::fromStdString(currency.iso_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(isoCode).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (clientManager_) {
        detailDialog->setClientManager(clientManager_);
        detailDialog->setUsername(username_.toStdString());
    }
    if (imageCache_) {
        detailDialog->setImageCache(imageCache_);
    }
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    // Connect revert signal
    connect(detailDialog, &CurrencyDetailDialog::revertRequested,
            this, &CurrencyController::onRevertCurrency);

    // Try to get history from the sender (history dialog) for version navigation
    auto* historyDialog = qobject_cast<CurrencyHistoryDialog*>(sender());
    if (historyDialog && !historyDialog->getHistory().versions.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Using history from sender for version navigation";
        detailDialog->setHistory(historyDialog->getHistory(), versionNumber);
    } else {
        // Fallback: just show single version without navigation
        detailDialog->setCurrency(currency);
        detailDialog->setReadOnly(true, versionNumber);
    }

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency: %1 (Version %2 - Read Only)")
        .arg(isoCode).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg", iconColor));

    // Track this version window
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
    // Set window flags AFTER addSubWindow (Qt resets flags when adding to MDI)
    detailWindow->setWindowFlags(detailWindow->windowFlags()
        & ~Qt::WindowMaximizeButtonHint);
    detailWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        detailWindow->show();
        detailWindow->detach();

        QPoint parentPos = currencyListWindow_->pos();
        detailWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        detailWindow->show();
    }
}

void CurrencyController::onRevertCurrency(const risk::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency: " << currency.iso_code;

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Revert requested but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Create a copy of the currency for saving
    risk::domain::currency currencyToSave = currency;
    currencyToSave.recorded_by = username_.toStdString();

    QPointer<CurrencyController> self = this;
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, currencyToSave]() -> std::pair<bool, std::string> {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save currency request for revert: "
                                       << currencyToSave.iso_code;

            risk::messaging::save_currency_request request{currencyToSave};
            auto payload = request.serialize();
            frame request_frame = frame(message_type::save_currency_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result)
                return {false, "Failed to communicate with server"};

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result)
                return {false, "Failed to decompress server response"};

            using risk::messaging::save_currency_response;
            auto response = save_currency_response::deserialize(*payload_result);

            bool result = false;
            std::string message = "Invalid server response";
            if (response) {
                result = response->success;
                message = response->message;
            }

            return {result, message};
        });

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, self,
        [self, watcher, currencyToSave]() {

        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Currency reverted successfully.";
            emit self->statusMessage(QString("Successfully reverted currency: %1")
                .arg(QString::fromStdString(currencyToSave.iso_code)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency revert failed: " << message;
            emit self->errorMessage(QString("Failed to revert currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self->mainWindow_, "Revert Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

}
