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
#include "ores.reporting.service/app/cut_authorisation_reconciler.hpp"
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <chrono>
#include <exception>
#include <utility>

namespace ores::reporting::service::app {

using namespace ores::logging;

cut_authorisation_reconciler::cut_authorisation_reconciler(
    std::uint32_t interval_seconds, std::shared_ptr<service::cut_authorisation_applier> applier)
    : interval_seconds_(interval_seconds)
    , applier_(std::move(applier)) {}

int cut_authorisation_reconciler::sweep() {
    try {
        return applier_->apply();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "The cut authorisation reconciliation failed; the next sweep tries again: "
            << e.what();
        return 0;
    }
}

boost::asio::awaitable<void> cut_authorisation_reconciler::run() {
    BOOST_LOG_SEV(lg(), info) << "Cut authorisation reconciliation started. Sweeping every "
                              << interval_seconds_ << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            sweep();

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted)
            BOOST_LOG_SEV(lg(), warn) << "Cut authorisation reconciliation timer error: "
                                      << e.what();
    }

    BOOST_LOG_SEV(lg(), info) << "Cut authorisation reconciliation stopped.";
}

}
