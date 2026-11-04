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
#ifndef ORES_HTTP_CORE_STREAM_CHANGE_TICKER_HPP
#define ORES_HTTP_CORE_STREAM_CHANGE_TICKER_HPP

#include "ores.http.core/export.hpp"
#include "ores.http.core/stream/change_pump.hpp"
#include <boost/asio/any_io_executor.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/steady_timer.hpp>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <functional>
#include <memory>
#include <mutex>

namespace ores::http::stream {

/**
 * @brief Closes the window a change opened.
 *
 * A window is what the coalescer gathered since it was armed. This is the clock that
 * ends it, and it is armed by the first change rather than re-armed on an interval:
 * a burst that begins just after a tick would otherwise be cut in two, and a screen
 * would redraw twice for one import.
 *
 * A window is never extended by what arrives inside it, so a long import is still a
 * series of windows and not one. A quiet system arms nothing, so the clock costs one
 * sleeping timer and no thread.
 *
 * The change that arms a window arrives on the publish thread and the timer belongs
 * to the server's, so an arm is a post: the timer is only ever touched by the
 * executor that owns it.
 *
 * One ticker per coalescer, for as long as the coalescer lives: the ticker is the
 * clock the coalescer calls when a change opens a window.
 */
class ORES_HTTP_CORE_EXPORT change_ticker final {
public:
    change_ticker(change_coalescer& coalescer, change_hub& hub, std::chrono::milliseconds window);

    ~change_ticker();

    change_ticker(const change_ticker&) = delete;
    change_ticker& operator=(const change_ticker&) = delete;

    /// Runs the loop until stop() is called. One per process.
    boost::asio::awaitable<void> run(boost::asio::any_io_executor executor);

    /**
     * @brief Opens a window that ends one interval from now.
     *
     * Safe to call from any thread. A call that lands while a window is open does
     * nothing, because the window belongs to the change that opened it and what
     * arrives later does not lengthen it.
     */
    void arm();

    /// Ends the loop after the window in progress.
    void stop();

    /// Whether a tick is still scheduled.
    [[nodiscard]] bool running() const {
        return !stopped_;
    }

private:
    /// What a posted arm finds. Shared, so a post that outlives the ticker is harmless.
    struct window final {
        std::shared_ptr<boost::asio::steady_timer> timer;
        bool armed = false;
        bool done = false;
    };

    /// Posts @p change to the window's executor, if there is one to post to.
    void post(std::function<void(const std::shared_ptr<window>&)> change);

    change_pump pump_;
    change_coalescer& coalescer_;
    std::chrono::milliseconds window_;
    std::shared_ptr<window> open_;
    std::atomic<bool> stopped_{false};
    mutable std::mutex mu_;
    std::shared_ptr<boost::asio::any_io_executor> executor_;
};

}

#endif
