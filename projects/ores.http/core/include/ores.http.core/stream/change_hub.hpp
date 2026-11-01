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
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::http::stream {

/**
 * @brief What one stream has been given and not yet written.
 *
 * The queue is bounded because a stream nobody reads must not grow without limit: a
 * client that has stopped reading, or a market that outpaces it, drops its oldest
 * frames instead of the server's memory. Dropping the oldest keeps the newest, which
 * is the half a screen can still use.
 *
 * A reader that is waiting for a frame registers a waker. A publisher that finds one
 * calls it, so a frame does not wait out a keep-alive interval before it is written.
 */
class change_subscription final {
public:
    explicit change_subscription(std::size_t capacity)
        : capacity_(capacity) {}

    change_subscription(const change_subscription&) = delete;
    change_subscription& operator=(const change_subscription&) = delete;

    /// Queues one frame, dropping the oldest when the queue is full.
    void push(std::string frame) {
        std::function<void()> waker;
        {
            const std::lock_guard lock(mu_);
            if (closed_)
                return;
            if (queue_.size() >= capacity_)
                queue_.pop_front();
            queue_.push_back(std::move(frame));
            waker = waker_;
        }
        if (waker)
            waker();
    }

    /// Takes the next frame, or nothing when none is waiting.
    std::optional<std::string> try_pop() {
        const std::lock_guard lock(mu_);
        if (queue_.empty())
            return std::nullopt;
        auto frame = std::move(queue_.front());
        queue_.pop_front();
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
            queue_.clear();
            waker = waker_;
            waker_ = nullptr;
        }
        if (waker)
            waker();
    }

    [[nodiscard]] bool closed() const {
        const std::lock_guard lock(mu_);
        return closed_;
    }

    [[nodiscard]] std::size_t size() const {
        const std::lock_guard lock(mu_);
        return queue_.size();
    }

private:
    mutable std::mutex mu_;
    std::deque<std::string> queue_;
    std::function<void()> waker_;
    std::size_t capacity_;
    bool closed_ = false;
};

/**
 * @brief The streams that are open, and the frames each one is owed.
 *
 * One hub per process. A publisher calls publish from whatever thread it is on; a
 * stream reads its own subscription from the connection's executor. The two meet on
 * the subscription's queue and nowhere else, so a slow reader costs its own queue
 * and not the publisher's thread.
 *
 * A frame is text the stream writes as it stands, because the framing belongs to the
 * route that knows it writes an event stream and not to the hub that fans out.
 */
class change_hub final {
public:
    explicit change_hub(std::size_t per_stream_capacity = 256)
        : capacity_(per_stream_capacity) {}

    [[nodiscard]] std::shared_ptr<change_subscription> subscribe() {
        const std::lock_guard lock(mu_);
        auto subscription = std::make_shared<change_subscription>(capacity_);
        subscriptions_.push_back(subscription);
        return subscription;
    }

    /// Gives every open stream the frame, and forgets the ones that have ended.
    void publish(std::string frame) {
        std::vector<std::shared_ptr<change_subscription>> open;
        {
            const std::lock_guard lock(mu_);
            std::erase_if(subscriptions_,
                          [](const auto& subscription) { return subscription->closed(); });
            open = subscriptions_;
        }
        for (auto& subscription : open)
            subscription->push(frame);
    }

    [[nodiscard]] std::size_t open_streams() const {
        const std::lock_guard lock(mu_);
        return subscriptions_.size();
    }

private:
    mutable std::mutex mu_;
    std::vector<std::shared_ptr<change_subscription>> subscriptions_;
    std::size_t capacity_;
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
            boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
            timer.expires_after(keep_alive_);
            subscription_->set_waker([&timer] { timer.cancel(); });
            boost::system::error_code ec;
            co_await timer.async_wait(boost::asio::redirect_error(boost::asio::use_awaitable, ec));
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
