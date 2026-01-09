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
#include "ores.comms/service/subscription_manager.hpp"

namespace ores::comms::service {

using namespace ores::logging;

void subscription_manager::register_session(const session_id& id,
                                            notification_callback callback) {
    std::lock_guard lock(mutex_);

    if (sessions_.contains(id)) {
        BOOST_LOG_SEV(lg(), warn)
            << "Session '" << id << "' already registered - updating callback";
    }

    sessions_[id] = session_info{
        .callback = std::move(callback),
        .subscribed_events = {}
    };

    BOOST_LOG_SEV(lg(), info)
        << "Registered session '" << id << "' (total sessions: "
        << sessions_.size() << ")";
}

void subscription_manager::unregister_session(const session_id& id) {
    std::lock_guard lock(mutex_);

    auto it = sessions_.find(id);
    if (it == sessions_.end()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot unregister unknown session: " << id;
        return;
    }

    // Remove from all event subscriber lists
    for (const auto& event_type : it->second.subscribed_events) {
        auto event_it = event_subscribers_.find(event_type);
        if (event_it != event_subscribers_.end()) {
            event_it->second.erase(id);
            if (event_it->second.empty()) {
                BOOST_LOG_SEV(lg(), debug)
                    << "No more subscribers for event type '" << event_type
                    << "' - removing from map";
                event_subscribers_.erase(event_it);
            }
        }
    }

    const auto subscription_count = it->second.subscribed_events.size();
    sessions_.erase(it);

    BOOST_LOG_SEV(lg(), info)
        << "Unregistered session '" << id << "' (had "
        << subscription_count << " subscriptions, remaining sessions: "
        << sessions_.size() << ")";
}

bool subscription_manager::subscribe(const session_id& id,
                                     const std::string& event_type) {
    std::lock_guard lock(mutex_);

    auto it = sessions_.find(id);
    if (it == sessions_.end()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot subscribe unknown session '" << id
            << "' to event type '" << event_type << "'";
        return false;
    }

    auto [_, inserted] = it->second.subscribed_events.insert(event_type);
    if (!inserted) {
        BOOST_LOG_SEV(lg(), debug)
            << "Session '" << id << "' already subscribed to '"
            << event_type << "'";
        return true; // Already subscribed is not an error
    }

    event_subscribers_[event_type].insert(id);

    const auto total_subscribers = event_subscribers_[event_type].size();
    BOOST_LOG_SEV(lg(), info)
        << "Session '" << id << "' subscribed to event type '"
        << event_type << "' (total subscribers for this event: "
        << total_subscribers << ")";

    return true;
}

bool subscription_manager::unsubscribe(const session_id& id,
                                       const std::string& event_type) {
    std::lock_guard lock(mutex_);

    auto session_it = sessions_.find(id);
    if (session_it == sessions_.end()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot unsubscribe unknown session '" << id
            << "' from event type '" << event_type << "'";
        return false;
    }

    auto erased = session_it->second.subscribed_events.erase(event_type);
    if (erased == 0) {
        BOOST_LOG_SEV(lg(), debug)
            << "Session '" << id << "' was not subscribed to '"
            << event_type << "'";
        return false;
    }

    std::size_t remaining = 0;
    auto event_it = event_subscribers_.find(event_type);
    if (event_it != event_subscribers_.end()) {
        event_it->second.erase(id);
        remaining = event_it->second.size();
        if (remaining == 0) {
            BOOST_LOG_SEV(lg(), debug)
                << "No more subscribers for event type '" << event_type
                << "' - removing from map";
            event_subscribers_.erase(event_it);
        }
    }
    BOOST_LOG_SEV(lg(), info)
        << "Session '" << id << "' unsubscribed from event type '"
        << event_type << "' (remaining subscribers: " << remaining << ")";

    return true;
}

std::size_t subscription_manager::notify(const std::string& event_type,
    std::chrono::system_clock::time_point timestamp,
    const std::vector<std::string>& entity_ids) {

    // Copy the data we need while holding the lock
    std::vector<std::pair<session_id, notification_callback>> callbacks_to_invoke;

    {
        std::lock_guard lock(mutex_);

        auto event_it = event_subscribers_.find(event_type);
        if (event_it == event_subscribers_.end()) {
            BOOST_LOG_SEV(lg(), debug)
                << "No subscribers for event type '" << event_type
                << "' - no notifications to send";
            return 0;
        }

        BOOST_LOG_SEV(lg(), info)
            << "Notifying " << event_it->second.size()
            << " subscriber(s) of event type '" << event_type
            << "' with " << entity_ids.size() << " entity IDs";

        for (const auto& session_id : event_it->second) {
            auto session_it = sessions_.find(session_id);
            if (session_it != sessions_.end()) {
                callbacks_to_invoke.emplace_back(
                    session_id, session_it->second.callback);
            }
        }
    }

    // Invoke callbacks without holding the lock
    std::size_t success_count = 0;
    for (const auto& [id, callback] : callbacks_to_invoke) {
        try {
            if (callback(event_type, timestamp, entity_ids)) {
                ++success_count;
                BOOST_LOG_SEV(lg(), debug)
                    << "Successfully notified session '" << id
                    << "' of event type '" << event_type << "'";
            } else {
                BOOST_LOG_SEV(lg(), warn)
                    << "Failed to notify session '" << id
                    << "' of event type '" << event_type
                    << "' - callback returned false";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Exception notifying session '" << id
                << "' of event type '" << event_type << "': " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info)
        << "Notification delivery complete for event type '" << event_type
        << "': " << success_count << "/" << callbacks_to_invoke.size()
        << " succeeded";

    return success_count;
}

std::size_t subscription_manager::subscriber_count(
    const std::string& event_type) const {
    std::lock_guard lock(mutex_);

    auto it = event_subscribers_.find(event_type);
    if (it == event_subscribers_.end()) {
        return 0;
    }
    return it->second.size();
}

std::size_t subscription_manager::session_count() const {
    std::lock_guard lock(mutex_);
    return sessions_.size();
}

std::vector<std::string> subscription_manager::get_subscriptions(
    const session_id& id) const {
    std::lock_guard lock(mutex_);

    auto it = sessions_.find(id);
    if (it == sessions_.end()) {
        return {};
    }

    return {it->second.subscribed_events.begin(),
            it->second.subscribed_events.end()};
}

}
