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
#ifndef ORES_EVENTING_SERVICE_EVENT_BUS_HPP
#define ORES_EVENTING_SERVICE_EVENT_BUS_HPP

#include <any>
#include <mutex>
#include <memory>
#include <vector>
#include <typeindex>
#include <functional>
#include <unordered_map>
#include <boost/core/demangle.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace ores::eventing::service {

/**
 * @brief RAII handle for managing event subscriptions.
 *
 * When destroyed, automatically unsubscribes from the event bus.
 * Can be moved but not copied.
 */
class subscription final {
public:
    using unsubscribe_fn = std::function<void()>;

    subscription() = default;
    explicit subscription(unsubscribe_fn fn) : unsubscribe_(std::move(fn)) {}

    ~subscription() {
        if (unsubscribe_) {
            unsubscribe_();
        }
    }

    subscription(const subscription&) = delete;
    subscription& operator=(const subscription&) = delete;

    subscription(subscription&& other) noexcept
        : unsubscribe_(std::move(other.unsubscribe_)) {
        other.unsubscribe_ = nullptr;
    }

    subscription& operator=(subscription&& other) noexcept {
        if (this != &other) {
            if (unsubscribe_) {
                unsubscribe_();
            }
            unsubscribe_ = std::move(other.unsubscribe_);
            other.unsubscribe_ = nullptr;
        }
        return *this;
    }

    /**
     * @brief Check if the subscription is active.
     */
    [[nodiscard]] bool is_active() const { return unsubscribe_ != nullptr; }

    /**
     * @brief Manually unsubscribe before destruction.
     */
    void unsubscribe() {
        if (unsubscribe_) {
            unsubscribe_();
            unsubscribe_ = nullptr;
        }
    }

private:
    unsubscribe_fn unsubscribe_;
};

/**
 * @brief A typed, thread-safe, in-process publish/subscribe event bus.
 *
 * The event bus provides a decoupled communication mechanism between components.
 * Publishers emit events without knowing who subscribes, and subscribers receive
 * events without knowing who publishes them.
 *
 * Thread Safety:
 * - All operations (publish, subscribe, unsubscribe) are thread-safe.
 * - Callbacks are invoked synchronously during publish(), so they should be
 *   non-blocking to avoid holding the lock for extended periods.
 *
 * Usage:
 * @code
 *     event_bus bus;
 *
 *     // Subscribe to an event type
 *     auto sub = bus.subscribe<my_event>([](const my_event& e) {
 *         // Handle event
 *     });
 *
 *     // Publish an event
 *     bus.publish(my_event{...});
 *
 *     // Subscription automatically cleaned up when 'sub' goes out of scope
 * @endcode
 */
class event_bus final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.eventing.service.event_bus");
        return instance;
    }

    using subscriber_id = std::uint64_t;

    struct subscriber_entry {
        subscriber_id id;
        std::any handler;
    };

    using subscriber_list = std::vector<subscriber_entry>;
    using subscriber_map = std::unordered_map<std::type_index, subscriber_list>;

public:
    event_bus() = default;
    ~event_bus() = default;

    event_bus(const event_bus&) = delete;
    event_bus& operator=(const event_bus&) = delete;
    event_bus(event_bus&&) = delete;
    event_bus& operator=(event_bus&&) = delete;

    /**
     * @brief Subscribe to events of a specific type.
     *
     * @tparam Event The event type to subscribe to.
     * @param handler The callback function to invoke when an event is published.
     * @return A subscription handle that unsubscribes when destroyed.
     *
     * @note The handler is invoked synchronously during publish(). It should be
     *       non-blocking to avoid holding the bus lock for extended periods.
     */
    template<typename Event>
    [[nodiscard]] subscription subscribe(std::function<void(const Event&)> handler) {
        using namespace ores::utility::log;
        std::lock_guard lock(mutex_);

        const auto type_idx = std::type_index(typeid(Event));
        const auto id = next_subscriber_id_++;
        const auto type_name = boost::core::demangle(type_idx.name());

        subscribers_[type_idx].push_back({id, std::move(handler)});

        const auto count = subscribers_[type_idx].size();
        BOOST_LOG_SEV(lg(), info)
            << "Subscriber " << id << " subscribed to event type '"
            << type_name << "' (total subscribers: " << count << ")";

        return subscription([this, type_idx, id, type_name]() {
            unsubscribe(type_idx, id, type_name);
        });
    }

    /**
     * @brief Publish an event to all subscribers.
     *
     * @tparam Event The event type being published.
     * @param event The event instance to publish.
     *
     * All registered handlers for this event type will be invoked synchronously.
     * Exceptions thrown by handlers are caught and logged, but do not prevent
     * other handlers from being called.
     */
    template<typename Event>
    void publish(const Event& event) {
        using namespace ores::utility::log;
        std::vector<std::function<void(const Event&)>> handlers_copy;
        const auto type_idx = std::type_index(typeid(Event));
        const auto type_name = boost::core::demangle(type_idx.name());

        {
            std::lock_guard lock(mutex_);

            auto it = subscribers_.find(type_idx);
            if (it == subscribers_.end()) {
                BOOST_LOG_SEV(lg(), debug)
                    << "No subscribers for event type '" << type_name
                    << "' - event will not be delivered";
                return;
            }

            handlers_copy.reserve(it->second.size());
            for (const auto& entry : it->second) {
                try {
                    const auto& handler =
                        std::any_cast<const std::function<void(const Event&)>&>(
                            entry.handler);
                    handlers_copy.push_back(handler);
                } catch (const std::bad_any_cast& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Type mismatch for subscriber " << entry.id
                        << " on event type '" << type_name << "': " << e.what();
                }
            }
        }

        BOOST_LOG_SEV(lg(), info)
            << "Publishing event type '" << type_name << "' to "
            << handlers_copy.size() << " subscriber(s)";

        std::size_t success_count = 0;
        for (const auto& handler : handlers_copy) {
            try {
                handler(event);
                ++success_count;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Exception in event handler for '" << type_name
                    << "': " << e.what();
            }
        }

        BOOST_LOG_SEV(lg(), debug)
            << "Event '" << type_name << "' delivery complete: "
            << success_count << "/" << handlers_copy.size() << " handlers succeeded";
    }

    /**
     * @brief Get the number of subscribers for a specific event type.
     *
     * @tparam Event The event type to query.
     * @return The number of active subscriptions for this event type.
     */
    template<typename Event>
    [[nodiscard]] std::size_t subscriber_count() const {
        std::lock_guard lock(mutex_);

        const auto type_idx = std::type_index(typeid(Event));
        auto it = subscribers_.find(type_idx);
        if (it == subscribers_.end()) {
            return 0;
        }
        return it->second.size();
    }

private:
    void unsubscribe(std::type_index type_idx, subscriber_id id,
                     const std::string& type_name) {
        using namespace ores::utility::log;
        std::lock_guard lock(mutex_);

        auto it = subscribers_.find(type_idx);
        if (it == subscribers_.end()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Unsubscribe called for subscriber " << id
                << " but no subscribers exist for event type '" << type_name << "'";
            return;
        }

        auto& list = it->second;
        auto removed = std::erase_if(list, [id](const subscriber_entry& entry) {
            return entry.id == id;
        });

        if (removed > 0) {
            const auto remaining = list.size();
            BOOST_LOG_SEV(lg(), info)
                << "Subscriber " << id << " unsubscribed from event type '"
                << type_name << "' (remaining subscribers: " << remaining << ")";
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Unsubscribe called for subscriber " << id
                << " but it was not found in event type '" << type_name << "'";
        }

        if (list.empty()) {
            BOOST_LOG_SEV(lg(), debug)
                << "No more subscribers for event type '" << type_name
                << "' - removing from map";
            subscribers_.erase(it);
        }
    }

    mutable std::mutex mutex_;
    subscriber_map subscribers_;
    subscriber_id next_subscriber_id_ = 1;
};

}

#endif
