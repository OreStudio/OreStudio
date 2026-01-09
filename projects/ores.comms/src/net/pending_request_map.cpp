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
#include "ores.comms/net/pending_request_map.hpp"

#include <boost/log/sources/record_ostream.hpp>

namespace ores::comms::net {

using namespace ores::logging;

pending_request_map::pending_request_map(boost::asio::any_io_executor executor)
    : executor_(std::move(executor)) {
}

std::shared_ptr<response_channel>
pending_request_map::register_request(std::uint32_t correlation_id) {
    auto channel = std::make_shared<response_channel>(executor_);

    std::lock_guard guard{mutex_};
    auto [it, inserted] = pending_.emplace(correlation_id, channel);
    if (!inserted) {
        BOOST_LOG_SEV(lg(), warn) << "Duplicate correlation_id: " << correlation_id
                                  << " - replacing existing channel";
        it->second = channel;
    }

    BOOST_LOG_SEV(lg(), trace) << "Registered pending request with correlation_id: "
                               << correlation_id << " (pending: " << pending_.size() << ")";
    return channel;
}

bool pending_request_map::complete(std::uint32_t correlation_id, messaging::frame response) {
    std::shared_ptr<response_channel> channel;
    {
        std::lock_guard guard{mutex_};
        auto it = pending_.find(correlation_id);
        if (it == pending_.end()) {
            BOOST_LOG_SEV(lg(), warn) << "No pending request for correlation_id: "
                                      << correlation_id;
            return false;
        }
        channel = it->second;
        pending_.erase(it);
    }

    BOOST_LOG_SEV(lg(), trace) << "Completing request with correlation_id: "
                               << correlation_id;
    channel->set_value(std::move(response));
    return true;
}

bool pending_request_map::fail(std::uint32_t correlation_id, ores::utility::serialization::error_code ec) {
    std::shared_ptr<response_channel> channel;
    {
        std::lock_guard guard{mutex_};
        auto it = pending_.find(correlation_id);
        if (it == pending_.end()) {
            BOOST_LOG_SEV(lg(), debug) << "No pending request for correlation_id: "
                                       << correlation_id << " (may already be completed)";
            return false;
        }
        channel = it->second;
        pending_.erase(it);
    }

    BOOST_LOG_SEV(lg(), debug) << "Failing request with correlation_id: "
                               << correlation_id << " error: " << ec;
    channel->set_error(ec);
    return true;
}

void pending_request_map::fail_all(ores::utility::serialization::error_code ec) {
    std::unordered_map<std::uint32_t, std::shared_ptr<response_channel>> pending_copy;
    {
        std::lock_guard guard{mutex_};
        pending_copy = std::move(pending_);
        pending_.clear();
    }

    if (!pending_copy.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Failing " << pending_copy.size()
                                  << " pending requests with error: " << ec;
    }

    for (auto& [id, channel] : pending_copy) {
        channel->set_error(ec);
    }
}

void pending_request_map::remove(std::uint32_t correlation_id) {
    std::lock_guard guard{mutex_};
    pending_.erase(correlation_id);
}

std::size_t pending_request_map::size() const {
    std::lock_guard guard{mutex_};
    return pending_.size();
}

bool pending_request_map::empty() const {
    std::lock_guard guard{mutex_};
    return pending_.empty();
}

}
