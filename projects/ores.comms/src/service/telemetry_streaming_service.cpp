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
#include "ores.comms/service/telemetry_streaming_service.hpp"

#include <boost/make_shared.hpp>
#include "ores.comms/net/client.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.telemetry/messaging/log_records_protocol.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"

namespace ores::comms::service {

using namespace ores::logging;

telemetry_streaming_service::telemetry_streaming_service(
    std::shared_ptr<net::client> client,
    telemetry_streaming_options options)
    : client_(std::move(client))
    , options_(std::move(options))
    , resource_(std::make_shared<telemetry::domain::resource>(
          telemetry::domain::resource::from_environment(
              options_.source_name, options_.source_version))) {

    BOOST_LOG_SEV(lg(), debug) << "Telemetry streaming service created for "
                               << options_.source_name;
}

telemetry_streaming_service::~telemetry_streaming_service() {
    stop();
}

void telemetry_streaming_service::start() {
    if (running_.exchange(true)) {
        BOOST_LOG_SEV(lg(), debug) << "Telemetry streaming already running";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Starting telemetry streaming service";

    // Create and register the sink
    auto backend = boost::make_shared<telemetry::log::telemetry_sink_backend>(
        resource_,
        [this](telemetry::domain::log_record record) {
            on_log_record(std::move(record));
        });

    sink_ = boost::make_shared<sink_t>(backend);
    boost::log::core::get()->add_sink(sink_);

    // Start the flush thread
    flush_thread_ = std::thread([this]() { flush_thread_func(); });

    BOOST_LOG_SEV(lg(), info) << "Telemetry streaming service started";
}

void telemetry_streaming_service::stop() {
    if (!running_.exchange(false)) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Stopping telemetry streaming service";

    // Wake up the flush thread
    flush_cv_.notify_one();

    // Wait for the flush thread to finish
    if (flush_thread_.joinable()) {
        flush_thread_.join();
    }

    // Final flush before unregistering sink
    flush();

    // Unregister the sink
    if (sink_) {
        boost::log::core::get()->remove_sink(sink_);
        sink_.reset();
    }

    BOOST_LOG_SEV(lg(), info) << "Telemetry streaming service stopped. "
                              << "Total sent: " << total_sent_
                              << ", dropped: " << total_dropped_;
}

bool telemetry_streaming_service::is_running() const noexcept {
    return running_.load();
}

void telemetry_streaming_service::flush() {
    std::vector<telemetry::domain::log_record> records_to_send;

    {
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        if (buffer_.empty()) {
            return;
        }
        records_to_send = std::move(buffer_);
        buffer_.clear();
    }

    if (!records_to_send.empty()) {
        if (!send_batch(records_to_send)) {
            // Failed to send - put records back if retry is enabled
            if (options_.retry_on_failure) {
                std::lock_guard<std::mutex> lock(buffer_mutex_);
                // Prepend failed records back to buffer
                buffer_.insert(buffer_.begin(),
                    std::make_move_iterator(records_to_send.begin()),
                    std::make_move_iterator(records_to_send.end()));
            } else {
                total_dropped_ += records_to_send.size();
            }
        }
    }
}

std::size_t telemetry_streaming_service::pending_count() const {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return buffer_.size();
}

std::uint64_t telemetry_streaming_service::total_sent() const noexcept {
    return total_sent_.load();
}

std::uint64_t telemetry_streaming_service::total_dropped() const noexcept {
    return total_dropped_.load();
}

void telemetry_streaming_service::on_log_record(
    telemetry::domain::log_record record) {

    // Filter out logs from telemetry-related components to prevent feedback loop.
    // These logs still go to file/console but are not sent to the server.
    const auto& name = record.logger_name;
    if (name.starts_with("ores.comms.service.telemetry") ||
        name.starts_with("ores.telemetry")) {
        return;
    }

    std::lock_guard<std::mutex> lock(buffer_mutex_);

    // Check if we need to drop old records
    if (buffer_.size() >= options_.max_pending_records) {
        // Drop oldest record
        buffer_.erase(buffer_.begin());
        ++total_dropped_;
    }

    buffer_.push_back(std::move(record));

    // Trigger immediate flush if batch is full
    if (buffer_.size() >= options_.batch_size) {
        flush_cv_.notify_one();
    }
}

void telemetry_streaming_service::flush_thread_func() {
    BOOST_LOG_SEV(lg(), debug) << "Flush thread started";

    while (running_.load()) {
        std::unique_lock<std::mutex> lock(buffer_mutex_);

        // Wait for batch full or timeout
        flush_cv_.wait_for(lock, options_.flush_interval, [this]() {
            return !running_.load() || buffer_.size() >= options_.batch_size;
        });

        if (!running_.load() && buffer_.empty()) {
            break;
        }

        // Extract records to send
        if (buffer_.empty()) {
            continue;
        }

        std::vector<telemetry::domain::log_record> records_to_send;
        records_to_send = std::move(buffer_);
        buffer_.clear();
        lock.unlock();

        // Send the batch
        if (!send_batch(records_to_send)) {
            // Failed - put back if retry enabled
            if (options_.retry_on_failure) {
                std::lock_guard<std::mutex> relock(buffer_mutex_);
                buffer_.insert(buffer_.begin(),
                    std::make_move_iterator(records_to_send.begin()),
                    std::make_move_iterator(records_to_send.end()));
            } else {
                total_dropped_ += records_to_send.size();
            }
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Flush thread exiting";
}

bool telemetry_streaming_service::send_batch(
    std::vector<telemetry::domain::log_record>& records) {

    if (records.empty()) {
        return true;
    }

    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Not connected, cannot send telemetry batch";
        return false;
    }

    BOOST_LOG_SEV(lg(), debug) << "Sending telemetry batch with "
                               << records.size() << " records";

    try {
        telemetry::messaging::submit_log_records_request request;
        request.records = std::move(records);

        auto result = client_->process_request<
            telemetry::messaging::submit_log_records_request,
            telemetry::messaging::submit_telemetry_response,
            messaging::message_type::submit_log_records_request>(
                std::move(request));

        if (!result) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to send telemetry batch: "
                                      << static_cast<int>(result.error());
            return false;
        }

        if (result->success) {
            total_sent_ += result->entries_accepted;
            BOOST_LOG_SEV(lg(), debug) << "Telemetry batch sent: "
                                       << result->entries_accepted << " accepted";
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Telemetry batch rejected: "
                                      << result->message;
            return false;
        }

        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Exception sending telemetry: " << e.what();
        return false;
    }
}

}
