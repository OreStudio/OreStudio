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
#ifndef ORES_HTTP_CORE_STREAM_CHANGE_HUB_HPP
#define ORES_HTTP_CORE_STREAM_CHANGE_HUB_HPP

#include "ores.http.api/domain/awaitable_body_source.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/redirect_error.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/error_code.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <limits>
#include <list>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::http::stream {

namespace detail {

/**
 * @brief The mark a hub puts on every id it issues.
 *
 * The process start, in nanoseconds, because an id from an earlier process must
 * never look like one of ours: a client that reconnects across a restart would
 * otherwise be replayed a gap that is not its own, and sequence numbers restart
 * with the process. A restart is exactly what this mark changes.
 */
inline std::string make_epoch() {
    const auto started = std::chrono::system_clock::now().time_since_epoch();
    return std::to_string(std::chrono::duration_cast<std::chrono::nanoseconds>(started).count());
}

}

/**
 * @brief Whose changes one stream hears.
 *
 * The tenant is the boundary a session may never cross. The party narrows it
 * within the tenant: a change to a party's data is the business of that party and
 * of an administrator; a tenant-wide change, which names no party, is everyone's.
 */
struct stream_scope final {
    std::string tenant;
    std::string party;
    bool sees_all_parties = false;
};

/**
 * @brief What one stream has been given and not yet written.
 *
 * One frame is held per collection, not per publication. A stream that fell behind
 * has missed the intermediate states of a collection but not the fact that it moved,
 * so a newer frame for a collection replaces the pending one rather than queueing
 * behind it. That is conflation: the payload says a collection moved, the client
 * refetches it, and the intermediate frames carried nothing the client could use.
 *
 * The batch is still bounded. When more collections are pending than the stream may
 * hold, the oldest one goes, because it is the frame the reader is least able to
 * use. A replaced frame moves to the back, so the ids a client reads keep ascending
 * and its Last-Event-ID stays a statement it can rely on.
 *
 * A reader that is waiting for a frame registers a waker. A publisher that finds one
 * calls it, so a frame does not wait out a keep-alive interval before it is written.
 */
class change_subscription final {
public:
    explicit change_subscription(stream_scope scope, std::size_t capacity)
        : scope_(std::move(scope))
        , capacity_(capacity) {}

    change_subscription(const change_subscription&) = delete;
    change_subscription& operator=(const change_subscription&) = delete;

    /**
     * @brief Holds one frame for its collection, replacing one that is still pending.
     *
     * @param collection What the frame is about. Two frames for the same collection
     *        and the same tenancy are two views of one fact, and the newer is the
     *        true one.
     */
    void push(std::string collection, std::string frame) {
        std::function<void()> waker;
        {
            const std::lock_guard lock(mu_);
            if (closed_)
                return;
            if (const auto replaced = pending_by_collection_.find(collection);
                replaced != pending_by_collection_.end()) {
                replaced->second->text = std::move(frame);
                pending_.splice(pending_.end(), pending_, replaced->second);
            } else {
                if (pending_.size() >= capacity_ && !pending_.empty()) {
                    pending_by_collection_.erase(pending_.front().collection);
                    pending_.pop_front();
                }
                pending_.push_back(pending_frame{std::move(collection), std::move(frame)});
                pending_by_collection_[pending_.back().collection] = std::prev(pending_.end());
            }
            waker = waker_;
        }
        if (waker)
            waker();
    }

    /// Takes the next frame, or nothing when none is waiting.
    std::optional<std::string> try_pop() {
        const std::lock_guard lock(mu_);
        if (pending_.empty())
            return std::nullopt;
        auto frame = std::move(pending_.front().text);
        pending_by_collection_.erase(pending_.front().collection);
        pending_.pop_front();
        return frame;
    }

    /// Registers what to call when a frame arrives, replacing any waker.
    void set_waker(std::function<void()> waker) {
        const std::lock_guard lock(mu_);
        waker_ = std::move(waker);
    }

    void clear_waker() {
        const std::lock_guard lock(mu_);
        waker_ = nullptr;
    }

    /// Stops the stream: nothing more is queued and the reader is told to end.
    void close() {
        std::function<void()> waker;
        {
            const std::lock_guard lock(mu_);
            closed_ = true;
            pending_.clear();
            pending_by_collection_.clear();
            waker = waker_;
            waker_ = nullptr;
        }
        if (waker)
            waker();
    }

    /// Whose changes this stream hears.
    [[nodiscard]] const stream_scope& scope() const {
        return scope_;
    }

    [[nodiscard]] bool closed() const {
        const std::lock_guard lock(mu_);
        return closed_;
    }

    [[nodiscard]] std::size_t size() const {
        const std::lock_guard lock(mu_);
        return pending_.size();
    }

private:
    struct pending_frame final {
        std::string collection;
        std::string text;
    };

    stream_scope scope_;
    mutable std::mutex mu_;
    std::list<pending_frame> pending_;
    std::unordered_map<std::string, std::list<pending_frame>::iterator> pending_by_collection_;
    std::function<void()> waker_;
    std::size_t capacity_;
    bool closed_ = false;
};

/**
 * @brief The streams that are open, and the frames each one is owed.
 *
 * One hub per process. A publisher calls publish from whatever thread it is on; a
 * stream reads its own subscription from the connection's executor. The two meet on
 * the subscription's pending batch and nowhere else, so a slow reader costs its own
 * buffer and not the publisher's thread.
 *
 * A frame is text the stream writes as it stands, because the framing belongs to the
 * route that knows it writes an event stream and not to the hub that fans out. The
 * hub is told what each frame is about, and no more, because that is what decides
 * which pending frame a newer one replaces.
 */
class change_hub final {
public:
    /**
     * @brief How many recent frames a reconnecting client may ask for.
     *
     * Core NATS has no replay, so the hub keeps a short window of what it
     * published. A client that says where it stopped and is inside the window is
     * given the frames it missed; one that is outside it is told to resync.
     */
    static constexpr std::size_t default_retention = 64;

    /// What a client's Last-Event-ID asks of the hub.
    struct resume_point final {
        /// The mark of the process that issued the id the client last saw.
        std::string epoch;
        /// How many frames that process had given the client when it stopped.
        std::uint64_t last_seen = 0;
    };

    /// The event-stream id this hub gives its @p sequence'th frame.
    [[nodiscard]] std::string id_of(std::uint64_t sequence) const {
        return epoch_ + '-' + std::to_string(sequence);
    }

    /**
     * @brief Reads an event-stream id, or nothing when it is not one of this form.
     *
     * An id is the mark of the process that issued it, a dash, and the frame's
     * sequence within that process. Anything else names no frame this hub could
     * place, so the caller answers it as it answers a gap: with one resync.
     */
    static std::optional<resume_point> read_id(std::string_view id) {
        const auto dash = id.find('-');
        if (dash == std::string_view::npos || dash == 0 || dash + 1 == id.size())
            return std::nullopt;

        std::uint64_t sequence = 0;
        for (const auto ch : id.substr(dash + 1)) {
            if (ch < '0' || ch > '9')
                return std::nullopt;
            const auto digit = static_cast<std::uint64_t>(ch - '0');
            if (sequence > (std::numeric_limits<std::uint64_t>::max() - digit) / 10)
                return std::nullopt;
            sequence = sequence * 10 + digit;
        }
        return resume_point{std::string(id.substr(0, dash)), sequence};
    }
    /**
     * @param system_tenant The tenant whose changes every stream hears, because they
     *        are the catalogue every tenant shares.
     */
    explicit change_hub(std::string system_tenant, std::size_t per_stream_capacity = 256)
        : system_tenant_(std::move(system_tenant))
        , epoch_(detail::make_epoch())
        , capacity_(per_stream_capacity) {}

    /// Opens a stream for @p tenant, which also hears the system tenant.
    /**
     * @param resumed The id the client last saw, when it reconnects. A client
     *        inside the retention window is given the frames it missed; one that
     *        is outside it, or that names an id this process never issued, is
     *        given one =resync= frame so it knows to reload rather than trust a
     *        silent gap.
     */
    [[nodiscard]] std::shared_ptr<change_subscription>
    subscribe(stream_scope scope, std::optional<resume_point> resumed = std::nullopt) {
        const std::lock_guard lock(mu_);
        auto subscription = std::make_shared<change_subscription>(scope, capacity_);

        const auto hears = [&](const std::string& tenant, const std::string& party) {
            if (tenant != scope.tenant && tenant != system_tenant_)
                return false;
            return party.empty() || scope.sees_all_parties || party == scope.party;
        };

        if (resumed) {
            const auto eligible = [&](const retained_frame& frame) {
                return frame.id > resumed->last_seen && hears(frame.tenant, frame.party);
            };
            const auto oldest = retained_.empty() ? 0 : retained_.front().id;
            const auto newest = retained_.empty() ? 0 : retained_.back().id;
            const auto inside = resumed->epoch == epoch_ && resumed->last_seen <= newest &&
                                (oldest == 0 || resumed->last_seen + 1 >= oldest);
            if (inside) {
                for (const auto& frame : retained_) {
                    if (eligible(frame))
                        subscription->push(
                            conflation_key(frame.tenant, frame.party, frame.collection),
                            "id: " + id_of(frame.id) + "\n" + frame.text);
                }
            } else {
                // No publish builds this key: every one it builds holds a separator.
                subscription->push("resync", "event: resync\ndata: {}\n\n");
            }
        }

        subscriptions_.push_back(subscription);
        return subscription;
    }

    /**
     * @brief Gives the frame to every open stream that hears what owns it.
     *
     * @param collection What the frame is about, as the component and the
     *        collection it names. A stream holds one pending frame per
     *        collection, so a frame is the newest statement about its collection
     *        and not one of a series.
     */
    void publish(const std::string& tenant,
                 const std::string& party,
                 const std::string& collection,
                 std::string frame) {
        std::vector<std::shared_ptr<change_subscription>> open;
        std::uint64_t id = 0;
        {
            const std::lock_guard lock(mu_);
            std::erase_if(subscriptions_,
                          [](const auto& subscription) { return subscription->closed(); });
            id = next_id_++;
            retained_.push_back(retained_frame{id, tenant, party, collection, frame});
            while (retained_.size() > retention_)
                retained_.pop_front();
            open = subscriptions_;
        }
        auto stamped = "id: " + id_of(id) + "\n" + frame;
        const auto key = conflation_key(tenant, party, collection);
        for (auto& subscription : open) {
            const auto& scope = subscription->scope();
            const auto hears = (tenant == scope.tenant || tenant == system_tenant_) &&
                               (party.empty() || scope.sees_all_parties || party == scope.party);
            if (hears)
                subscription->push(key, stamped);
        }
    }

    /// How many streams are still open. A closed one is not counted, even before
    /// the next publish gets around to forgetting it.
    [[nodiscard]] std::size_t open_streams() const {
        const std::lock_guard lock(mu_);
        return static_cast<std::size_t>(std::count_if(
            subscriptions_.begin(), subscriptions_.end(), [](const auto& subscription) {
                return !subscription->closed();
            }));
    }

private:
    struct retained_frame final {
        std::uint64_t id = 0;
        std::string tenant;
        std::string party;
        std::string collection;
        std::string text;
    };

    /**
     * @brief What makes two frames the same fact.
     *
     * A stream may hear two tenants and several parties, so the collection alone
     * would let one tenant's frame replace another's. The parts cannot contain the
     * separator: a tenant and a party are identifiers and a collection is a
     * component and a name.
     */
    static std::string conflation_key(const std::string& tenant,
                                      const std::string& party,
                                      const std::string& collection) {
        return tenant + '/' + party + '/' + collection;
    }

    std::string system_tenant_;
    std::string epoch_;
    mutable std::mutex mu_;
    std::vector<std::shared_ptr<change_subscription>> subscriptions_;
    std::size_t capacity_;
    std::size_t retention_ = default_retention;
    std::uint64_t next_id_ = 1;
    std::deque<retained_frame> retained_;
};

/**
 * @brief A response body written from a subscription.
 *
 * It waits for a frame, and when none comes within the keep-alive interval it writes
 * a comment line instead. The comment keeps a stream truthful through a proxy that
 * would otherwise time it out, and it resets the idle timer of a connection that is
 * merely quiet.
 *
 * The source ends when its subscription closes, which is how a caller stops a stream
 * it opened.
 */
class change_stream_source final : public domain::awaitable_body_source {
public:
    change_stream_source(std::shared_ptr<change_subscription> subscription,
                         std::chrono::milliseconds keep_alive)
        : subscription_(std::move(subscription))
        , keep_alive_(keep_alive) {}

    /**
     * @brief Ends the stream when the client it was written for is gone.
     *
     * The hub holds its own reference to the subscription, so a source that only
     * dropped its own would leave the stream in the hub for the life of the
     * process: its queue would keep filling with every frame, for a reader that no
     * longer exists. Closing here is what lets the next publish forget it.
     */
    ~change_stream_source() override {
        if (subscription_)
            subscription_->close();
    }

    [[nodiscard]] std::optional<std::uint64_t> size() const override {
        return std::nullopt;
    }

    boost::asio::awaitable<std::size_t> read(std::span<char> into) override {
        for (;;) {
            if (!ready_.empty())
                co_return take_from_ready(into);

            if (subscription_->closed())
                co_return 0;

            if (auto frame = subscription_->try_pop(); frame.has_value()) {
                ready_.push_back(std::move(*frame));
                continue;
            }

            /*
             * Nothing to write, so wait for the interval. The waker cancels the
             * timer when a frame arrives, so a frame does not wait it out; a cancel
             * that lands before the wait begins does nothing and costs one interval.
             */
            /*
             * The timer is held by shared ownership because the waker outlives this
             * scope when the read is cancelled: a cancel that lands on a destroyed
             * timer would be a call into freed memory, and this makes it a no-op.
             */
            auto timer = std::make_shared<boost::asio::steady_timer>(
                co_await boost::asio::this_coro::executor);
            timer->expires_after(keep_alive_);
            subscription_->set_waker([timer] { timer->cancel(); });
            boost::system::error_code ec;
            co_await timer->async_wait(boost::asio::redirect_error(boost::asio::use_awaitable, ec));
            subscription_->clear_waker();

            if (!subscription_->closed() && subscription_->size() == 0)
                ready_.emplace_back(": keep-alive\n\n");
        }
    }

    std::size_t take_from_ready(std::span<char> into) {
        auto& front = ready_.front();
        const auto take = std::min(into.size(), front.size());
        std::copy_n(front.data(), take, into.data());
        if (take == front.size())
            ready_.pop_front();
        else
            front.erase(0, take);
        return take;
    }

    std::shared_ptr<change_subscription> subscription_;
    std::chrono::milliseconds keep_alive_;
    std::deque<std::string> ready_;
};

}

#endif
