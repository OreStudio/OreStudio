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
#include "ores.inbox.service/app/approval_expiry_sweeper.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>

namespace ores::inbox::service::app {

using namespace ores::logging;

approval_expiry_sweeper::approval_expiry_sweeper(std::uint32_t interval_seconds,
                                                 ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , ctx_(std::move(ctx)) {}

void approval_expiry_sweeper::sweep_once() {
    approval_lifecycle lifecycle(ctx_);
    const auto expired = lifecycle.expire_overdue();
    if (!expired.empty())
        BOOST_LOG_SEV(lg(), info) << "Closed " << expired.size() << " approval request(s).";
}

boost::asio::awaitable<void> approval_expiry_sweeper::run() {
    BOOST_LOG_SEV(lg(), info) << "Approval expiry sweeper started. Sweeping every "
                              << interval_seconds_ << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            try {
                sweep_once();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Approval expiry sweep failed: " << e.what();
            }

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn) << "Approval expiry sweeper timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Approval expiry sweeper stopped.";
}

}
