/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CountryController.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/CountryMdiWindow.hpp"
#include "ores.qt/CountryDetailDialog.hpp"
#include "ores.qt/CountryHistoryDialog.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.refdata/eventing/country_changed_event.hpp"
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    // Event type name for country changes
    constexpr std::string_view country_event_name =
        eventing::domain::event_traits<refdata::eventing::country_changed_event>::name;
}

CountryController::CountryController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, {}, parent),
      imageCache_(imageCache),
      changeReasonCache_(changeReasonCache),
      countryListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Country controller created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CountryController::onNotificationReceived);

        // Subscribe to events when logged in (event adapter only available after login)
        connect(clientManager_, &ClientManager::loggedIn,
                this, [self = QPointer<CountryController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Subscribing to country change events";
            self->clientManager_->subscribeToEvent(std::string{country_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [self = QPointer<CountryController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to country change events after reconnect";
            self->clientManager_->subscribeToEvent(std::string{country_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to country change events";
            clientManager_->subscribeToEvent(std::string{country_event_name});
        }
    }
}

CountryController::~CountryController() {
    BOOST_LOG_SEV(lg(), debug) << "Country controller destroyed";

    // Unsubscribe from country change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from country change events";
        clientManager_->unsubscribeFromEvent(std::string{country_event_name});
    }
}

void CountryController::showListWindow() {
    // Reuse existing window if it exists
    if (countryListWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing countries window";

        // Bring window to front
        if (countryListWindow_->isDetached()) {
            countryListWindow_->setVisible(true);
            countryListWindow_->show();
            countryListWindow_->raise();
            countryListWindow_->activateWindow();
        } else {
            countryListWindow_->setVisible(true);
            mdiArea_->setActiveSubWindow(countryListWindow_);
            countryListWindow_->show();
            countryListWindow_->raise();
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new countries MDI window";
    auto* countryWidget = new CountryMdiWindow(clientManager_, imageCache_,
                                                username_, mainWindow_);

    // Connect status signals
    connect(countryWidget, &CountryMdiWindow::statusChanged,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(countryWidget, &CountryMdiWindow::errorOccurred,
            this, [self = QPointer<CountryController>(this)](const QString& err_msg) {
        if (!self) return;
        emit self->errorMessage("Error loading countries: " + err_msg);
    });

    // Connect country operations (add, edit, history)
    connect(countryWidget, &CountryMdiWindow::addNewRequested,
            this, &CountryController::onAddNewRequested);
    connect(countryWidget, &CountryMdiWindow::showCountryDetails,
            this, &CountryController::onShowCountryDetails);
    connect(countryWidget, &CountryMdiWindow::showCountryHistory,
            this, &CountryController::onShowCountryHistory);

    countryListWindow_ = new DetachableMdiSubWindow();
    countryListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    countryListWindow_->setWidget(countryWidget);
    countryListWindow_->setWindowTitle("Countries");
    countryListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Globe, IconUtils::DefaultIconColor));

    // Track window for detach/reattach operations
    register_detachable_window(countryListWindow_);
    QPointer<CountryController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = countryListWindow_;
    // to avoid race conditions with signal processing
    connect(countryListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed](QObject* obj) {
            Q_UNUSED(obj);
            if (!self) return;

            BOOST_LOG_SEV(lg(), debug) << "Detachable MDI Sub Window destroyed";

            // If the destroyed window is the country list, nullify the pointer
            if (self->countryListWindow_ == windowBeingDestroyed)
                self->countryListWindow_ = nullptr;
        });

    // When the image cache reloads (due to external event), mark the list as stale
    connect(imageCache_, &ImageCache::allLoaded, this, [self = QPointer<CountryController>(this), countryWidget](){
        if (!self) return;
        if (countryWidget) {
            countryWidget->markAsStale();
        }
    });

    mdiArea_->addSubWindow(countryListWindow_);
    countryListWindow_->adjustSize();
    countryListWindow_->show();
}

void CountryController::closeAllWindows() {
    if (countryListWindow_) {
        countryListWindow_->close();
    }
}

void CountryController::reloadListWindow() {
    if (countryListWindow_) {
        if (auto* widget = qobject_cast<CountryMdiWindow*>(countryListWindow_->widget())) {
            widget->reload();
        }
    }
}

void CountryController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new country requested";
    refdata::domain::country new_country;

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
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

    connect(detailDialog, &CountryDetailDialog::statusMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &CountryDetailDialog::errorMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    detailDialog->setCountry(new_country);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Country");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Globe, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, countryListWindow_);
}

void CountryController::onShowCountryDetails(
    const refdata::domain::country& country) {
    BOOST_LOG_SEV(lg(), info) << "Showing country details for: "
                             << country.alpha2_code;

    const QString alpha2Code = QString::fromStdString(country.alpha2_code);
    const QString windowKey = build_window_key("details", alpha2Code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing detail window for: "
                                  << country.alpha2_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new detail window for: "
                              << country.alpha2_code;

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
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

    connect(detailDialog, &CountryDetailDialog::statusMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &CountryDetailDialog::errorMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    detailDialog->setCountry(country);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Country Details: %1").arg(alpha2Code));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Globe, IconUtils::DefaultIconColor));

    // Track this detail window
    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CountryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, countryListWindow_);
}

void CountryController::onShowCountryHistory(const QString& alpha2Code) {
    BOOST_LOG_SEV(lg(), info) << "Showing country history for: "
                             << alpha2Code.toStdString();

    const QString windowKey = build_window_key("history", alpha2Code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << alpha2Code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << alpha2Code.toStdString();

    auto* historyDialog = new CountryHistoryDialog(alpha2Code, clientManager_, mainWindow_);
    if (imageCache_) {
        historyDialog->setImageCache(imageCache_);
    }

    connect(historyDialog, &CountryHistoryDialog::statusChanged,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &CountryHistoryDialog::errorOccurred,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &CountryHistoryDialog::revertVersionRequested,
            this, &CountryController::onRevertCountry);
    connect(historyDialog, &CountryHistoryDialog::openVersionRequested,
            this, &CountryController::onOpenCountryVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Country History: %1").arg(alpha2Code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<CountryController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, countryListWindow_);
}

void CountryController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds, const QString& /*tenantId*/) {
    // Check if this is a country change event
    if (eventType != QString::fromStdString(std::string{country_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received country change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " alpha2 codes";

    // If the country list window is open, mark it as stale
    if (countryListWindow_) {
        auto* countryWidget = qobject_cast<CountryMdiWindow*>(
            countryListWindow_->widget());
        if (countryWidget) {
            countryWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked country window as stale";
        }
    }

    // Notify open detail and history dialogs for affected countries
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        const QString& key = it.key();
        auto* window = it.value();
        if (!window)
            continue;

        // Check if this is a detail window for an affected country
        if (key.startsWith("details:")) {
            QString windowAlpha2 = key.mid(8);  // Remove "details:" prefix
            if (entityIds.isEmpty() || entityIds.contains(windowAlpha2)) {
                // Mark detail dialog as stale
                auto* detailDialog = qobject_cast<CountryDetailDialog*>(
                    window->widget());
                if (detailDialog) {
                    detailDialog->markAsStale();
                    BOOST_LOG_SEV(lg(), debug) << "Marked detail dialog as stale for: "
                                               << windowAlpha2.toStdString();
                }
            }
        }
        // Check if this is a history window for an affected country
        else if (key.startsWith("history:")) {
            QString windowAlpha2 = key.mid(8);  // Remove "history:" prefix
            if (entityIds.isEmpty() || entityIds.contains(windowAlpha2)) {
                // Mark history dialog as stale
                auto* historyDialog = qobject_cast<CountryHistoryDialog*>(
                    window->widget());
                if (historyDialog) {
                    historyDialog->markAsStale();
                    BOOST_LOG_SEV(lg(), debug) << "Marked history dialog as stale for: "
                                               << windowAlpha2.toStdString();
                }
            }
        }
    }
}

void CountryController::onOpenCountryVersion(
    const refdata::domain::country& country, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for country: " << country.alpha2_code;

    const QString alpha2Code = QString::fromStdString(country.alpha2_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(alpha2Code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
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

    connect(detailDialog, &CountryDetailDialog::statusMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &CountryDetailDialog::errorMessage,
            this, [self = QPointer<CountryController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    // Connect revert signal
    connect(detailDialog, &CountryDetailDialog::revertRequested,
            this, &CountryController::onRevertCountry);

    // Try to get history from the sender (history dialog) for version navigation
    auto* historyDialog = qobject_cast<CountryHistoryDialog*>(sender());
    if (historyDialog && !historyDialog->getHistory().empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Using history from sender for version navigation";
        detailDialog->setHistory(historyDialog->getHistory(), versionNumber);
    } else {
        // Fallback: just show single version without navigation
        detailDialog->setCountry(country);
        detailDialog->setReadOnly(true, versionNumber);
    }

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Country: %1 (Version %2 - Read Only)")
        .arg(alpha2Code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Globe, IconUtils::DefaultIconColor));

    // Track this version window
    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CountryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, countryListWindow_);
}

void CountryController::onRevertCountry(const refdata::domain::country& country) {
    BOOST_LOG_SEV(lg(), info) << "Reverting country: " << country.alpha2_code;

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Revert requested but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    refdata::domain::country countryToSave = country;
    countryToSave.modified_by = username_.toStdString();

    QPointer<CountryController> self = this;
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, countryToSave]() -> std::pair<bool, std::string> {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save country request for revert: "
                                       << countryToSave.alpha2_code;

            using comms::messaging::frame;
            using comms::messaging::message_type;
            using refdata::messaging::save_country_request;
            using refdata::messaging::save_country_response;

            save_country_request request{countryToSave};
            auto payload = request.serialize();
            frame request_frame = frame(message_type::save_country_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result)
                return {false, "Failed to communicate with server"};

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result)
                return {false, "Failed to decompress server response"};

            auto response = save_country_response::deserialize(*payload_result);

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
        [self, watcher, alpha2_code = country.alpha2_code]() {
            if (!self) {
                watcher->deleteLater();
                return;
            }

            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (success) {
                BOOST_LOG_SEV(lg(), info) << "Country reverted successfully: "
                                          << alpha2_code;
                emit self->statusMessage(QString("Successfully reverted country: %1")
                    .arg(QString::fromStdString(alpha2_code)));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Country revert failed: " << message;
                emit self->errorMessage(QString("Failed to revert country: %1")
                    .arg(QString::fromStdString(message)));
            }
        });

    watcher->setFuture(future);
}

}
