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
#include <chrono>
#include <cstddef>

namespace ores::http::stream {

/**
 * @brief Closes a window on an interval.
 *
 * A window is what the coalescer gathered since the last one. This is the clock that
 * ends it: one timer per process, re-armed after each tick, so a quiet system pays
 * one timer per interval and no thread.
 *
 * The loop ends when the ticker is stopped, which a draining server calls. It
 * publishes nothing after that, so a shutdown does not write to a stream it is about
 * to close.
 */
class ORES_HTTP_CORE_EXPORT change_ticker final {
public:
    change_ticker(change_coalescer& coalescer, change_hub& hub, std::chrono::milliseconds interval)
        : pump_(coalescer, hub)
        , interval_(interval) {}

    /// Runs the loop until stop() is called. One per process.
    boost::asio::awaitable<void> run(boost::asio::any_io_executor executor);

    /// Ends the loop after the tick in progress.
    void stop();

    /// Whether a tick is still scheduled.
    [[nodiscard]] bool running() const {
        return !stopped_;
    }

private:
    change_pump pump_;
    std::chrono::milliseconds interval_;
    bool stopped_ = false;
};

}

#endif
