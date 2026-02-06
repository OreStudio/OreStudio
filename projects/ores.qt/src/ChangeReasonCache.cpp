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
#include "ores.qt/ChangeReasonCache.hpp"

#include <algorithm>
#include <QtConcurrent>
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/error_protocol.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/change_reason_changed_event.hpp"
#include "ores.dq/eventing/change_reason_category_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    constexpr std::string_view reason_event_name =
        eventing::domain::event_traits<
            dq::eventing::change_reason_changed_event>::name;
    constexpr std::string_view category_event_name =
        eventing::domain::event_traits<
            dq::eventing::change_reason_category_changed_event>::name;
}

ChangeReasonCache::ChangeReasonCache(ClientManager* clientManager,
    QObject* parent)
    : QObject(parent),
      clientManager_(clientManager),
      reasons_watcher_(new QFutureWatcher<ReasonsResult>(this)),
      categories_watcher_(new QFutureWatcher<CategoriesResult>(this)) {

    connect(reasons_watcher_, &QFutureWatcher<ReasonsResult>::finished,
            this, &ChangeReasonCache::onReasonsLoaded);
    connect(categories_watcher_, &QFutureWatcher<CategoriesResult>::finished,
            this, &ChangeReasonCache::onCategoriesLoaded);

    // Subscribe to change events
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &ChangeReasonCache::onNotificationReceived);

        connect(clientManager_, &ClientManager::loggedIn,
                this, &ChangeReasonCache::subscribeToEvents);
        connect(clientManager_, &ClientManager::reconnected,
                this, &ChangeReasonCache::subscribeToEvents);

        // Also subscribe if already logged in
        if (clientManager_->isLoggedIn()) {
            subscribeToEvents();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonCache created";
}

void ChangeReasonCache::subscribeToEvents() {
    if (!clientManager_)
        return;

    BOOST_LOG_SEV(lg(), info) << "Subscribing to change reason events";
    clientManager_->subscribeToEvent(std::string{reason_event_name});
    clientManager_->subscribeToEvent(std::string{category_event_name});
}

void ChangeReasonCache::loadAll() {
    if (is_loading_) {
        BOOST_LOG_SEV(lg(), debug) << "Already loading, skipping";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading change reasons and categories";
    is_loading_ = true;
    reasons_loaded_ = false;
    categories_loaded_ = false;

    loadReasons();
    loadCategories();
}

void ChangeReasonCache::loadReasons() {
    QPointer<ChangeReasonCache> self = this;

    QFuture<ReasonsResult> future = QtConcurrent::run([self]() -> ReasonsResult {
        if (!self || !self->clientManager_) {
            return {false, {}};
        }

        dq::messaging::get_change_reasons_request request;
        auto payload = request.serialize();

        frame request_frame(
            message_type::get_change_reasons_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send reasons request";
            return {false, {}};
        }

        // Check for error response
        if (response_result->header().type == message_type::error_response) {
            auto err_payload = response_result->decompressed_payload();
            if (err_payload) {
                auto err_resp = comms::messaging::error_response::deserialize(*err_payload);
                if (err_resp) {
                    BOOST_LOG_SEV(lg(), error) << "Server returned error for reasons request: "
                                               << err_resp->message;
                }
            }
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress reasons response";
            return {false, {}};
        }

        auto response = dq::messaging::get_change_reasons_response::
            deserialize(*payload_result);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize reasons response";
            return {false, {}};
        }

        BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->reasons.size()
                                   << " change reasons";
        return {true, std::move(response->reasons)};
    });

    reasons_watcher_->setFuture(future);
}

void ChangeReasonCache::loadCategories() {
    QPointer<ChangeReasonCache> self = this;

    QFuture<CategoriesResult> future = QtConcurrent::run([self]() -> CategoriesResult {
        if (!self || !self->clientManager_) {
            return {false, {}};
        }

        dq::messaging::get_change_reason_categories_request request;
        auto payload = request.serialize();

        frame request_frame(
            message_type::get_change_reason_categories_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send categories request";
            return {false, {}};
        }

        // Check for error response
        if (response_result->header().type == message_type::error_response) {
            auto err_payload = response_result->decompressed_payload();
            if (err_payload) {
                auto err_resp = comms::messaging::error_response::deserialize(*err_payload);
                if (err_resp) {
                    BOOST_LOG_SEV(lg(), error) << "Server returned error for categories request: "
                                               << err_resp->message;
                }
            }
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress categories response";
            return {false, {}};
        }

        auto response = dq::messaging::get_change_reason_categories_response::
            deserialize(*payload_result);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize categories response";
            return {false, {}};
        }

        BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->categories.size()
                                   << " change reason categories";
        return {true, std::move(response->categories)};
    });

    categories_watcher_->setFuture(future);
}

void ChangeReasonCache::onReasonsLoaded() {
    auto result = reasons_watcher_->result();
    if (!result.success) {
        is_loading_ = false;
        emit loadError("Failed to fetch change reasons");
        return;
    }

    reasons_ = std::move(result.reasons);

    // Build index for fast lookup
    reason_index_.clear();
    for (std::size_t i = 0; i < reasons_.size(); ++i) {
        reason_index_[reasons_[i].code] = i;
    }

    BOOST_LOG_SEV(lg(), info) << "Cached " << reasons_.size()
                              << " change reasons";

    reasons_loaded_ = true;
    if (categories_loaded_) {
        is_loading_ = false;
        is_loaded_ = true;
        emit loaded();
    }
}

void ChangeReasonCache::onCategoriesLoaded() {
    auto result = categories_watcher_->result();
    if (!result.success) {
        is_loading_ = false;
        emit loadError("Failed to fetch change reason categories");
        return;
    }

    categories_ = std::move(result.categories);
    BOOST_LOG_SEV(lg(), info) << "Cached " << categories_.size()
                              << " change reason categories";

    categories_loaded_ = true;
    if (reasons_loaded_) {
        is_loading_ = false;
        is_loaded_ = true;
        emit loaded();
    }
}

void ChangeReasonCache::onNotificationReceived(const QString& eventType,
    const QDateTime& timestamp, const QStringList& entityIds,
    const QString& /*tenantId*/) {

    const auto reason_event = QString::fromStdString(std::string{reason_event_name});
    const auto category_event = QString::fromStdString(std::string{category_event_name});

    if (eventType == reason_event || eventType == category_event) {
        BOOST_LOG_SEV(lg(), info) << "Received change notification, refreshing cache";

        // Reload everything
        is_loaded_ = false;
        loadAll();

        // Emit refreshed after load completes
        connect(this, &ChangeReasonCache::loaded, this, [this]() {
            emit refreshed();
        }, Qt::SingleShotConnection);
    }
}

std::vector<dq::domain::change_reason> ChangeReasonCache::getReasonsForAmend(
    const std::string& category_code) const {

    std::vector<dq::domain::change_reason> result;
    for (const auto& reason : reasons_) {
        if (reason.applies_to_amend && reason.category_code == category_code) {
            result.push_back(reason);
        }
    }

    // Sort by display_order
    std::sort(result.begin(), result.end(),
        [](const auto& a, const auto& b) {
            return a.display_order < b.display_order;
        });

    return result;
}

std::vector<dq::domain::change_reason> ChangeReasonCache::getReasonsForDelete(
    const std::string& category_code) const {

    std::vector<dq::domain::change_reason> result;
    for (const auto& reason : reasons_) {
        if (reason.applies_to_delete && reason.category_code == category_code) {
            result.push_back(reason);
        }
    }

    // Sort by display_order
    std::sort(result.begin(), result.end(),
        [](const auto& a, const auto& b) {
            return a.display_order < b.display_order;
        });

    return result;
}

const dq::domain::change_reason* ChangeReasonCache::getReasonByCode(
    const std::string& code) const {

    auto it = reason_index_.find(code);
    if (it == reason_index_.end()) {
        return nullptr;
    }
    return &reasons_[it->second];
}

bool ChangeReasonCache::isValidReasonCode(const std::string& code) const {
    return reason_index_.find(code) != reason_index_.end();
}

}
