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
#include "ores.comms/connection.hpp"
#include <boost/asio/read.hpp>
#include <boost/asio/write.hpp>
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.connection"));

}

namespace ores::comms {

connection::connection(ssl_socket socket) : socket_(std::move(socket)) {}

cobalt::promise<void> connection::ssl_handshake_server() {
    co_await socket_.async_handshake(ssl::stream_base::server, cobalt::use_op);
}

cobalt::promise<void> connection::ssl_handshake_client() {
    co_await socket_.async_handshake(ssl::stream_base::client, cobalt::use_op);
}

cobalt::promise<std::expected<protocol::frame, protocol::error_code>> connection::read_frame() {
    try {
        BOOST_LOG_SEV(lg, debug) << "Starting to read frame...";
        
        // We need to peek at the beginning of the BSON document to understand its structure
        // The frame serialization writes the header as BSON, then the payload separately
        // This means we need to read the BSON header size first, then parse it to know payload size
        
        // First, read at least 4 bytes for a possible BSON document length
        std::array<uint8_t, 4> length_buffer;
        co_await boost::asio::async_read(
            socket_,
            boost::asio::buffer(length_buffer),
            cobalt::use_op);

        // Read the BSON document length (first 4 bytes, little-endian)
        uint32_t bson_length = 0;
        bson_length |= static_cast<uint32_t>(length_buffer[0]);
        bson_length |= static_cast<uint32_t>(length_buffer[1]) << 8;
        bson_length |= static_cast<uint32_t>(length_buffer[2]) << 16;
        bson_length |= static_cast<uint32_t>(length_buffer[3]) << 24;

        BOOST_LOG_SEV(lg, debug) << "Read BSON document length: " << bson_length;
        
        // Sanity check
        if (bson_length < 4 || bson_length > 1024 * 1024) {  // Max 1MB
            BOOST_LOG_SEV(lg, error) << "Invalid BSON document length: " << bson_length;
            co_return std::unexpected(protocol::error_code::invalid_message_type);
        }

        // Read the complete BSON document (header part)
        std::vector<uint8_t> bson_header_part(bson_length);
        std::copy(length_buffer.begin(), length_buffer.end(), bson_header_part.begin());

        // Read the rest of the BSON document
        co_await boost::asio::async_read(
            socket_,
            boost::asio::buffer(bson_header_part.data() + 4, bson_length - 4),
            cobalt::use_op);

        // Parse this BSON header part to extract the payload size
        // We need to deserialize just this part to get the frame header
        // But we have a problem - the deserialize function expects the full frame (header + payload)
        // Instead, we need to deserialize the BSON part to get the payload size, then read the payload
        
        // Let's manually extract the payload_size from the BSON
        // This is tricky because BSON is a complex format
        // A better approach: temporarily deserialize the header to get payload size
        // But this requires the frame::deserialize to be able to work with just the header part
        // This suggests that the current frame design has an issue
        
        // Actually, let me reconsider the approach:
        // The frame serialization combines BSON header + raw payload
        // So when reading, we need to:
        // 1. Read enough to determine the BSON header size
        // 2. Read the full BSON header 
        // 3. Parse it to get the payload size
        // 4. Read the payload
        // 5. Combine them and deserialize as a frame
        
        // For now, let's use a more practical approach by reading a reasonable chunk
        // and using our existing frame::deserialize to determine the split
        // Actually, let me just read more data and try to deserialize it chunk by chunk
        
        // Let's create a temporary header frame to understand the payload size
        std::string bson_str(reinterpret_cast<const char*>(bson_header_part.data()), bson_header_part.size());
        auto temp_header_result = rfl::bson::read<protocol::frame_header>(bson_str);
        
        if (!temp_header_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize frame header from BSON bytes";
            co_return std::unexpected(protocol::error_code::invalid_message_type);
        }
        
        uint32_t payload_size = temp_header_result.value().payload_size;
        BOOST_LOG_SEV(lg, debug) << "Extracted payload size: " << payload_size;

        // Read the payload
        std::vector<uint8_t> payload_buffer;
        if (payload_size > 0) {
            payload_buffer.resize(payload_size);
            co_await boost::asio::async_read(
                socket_,
                boost::asio::buffer(payload_buffer),
                cobalt::use_op);
        }

        // Combine the BSON header part and payload to create full frame data
        std::vector<uint8_t> full_frame_data;
        full_frame_data.reserve(bson_header_part.size() + payload_buffer.size());
        full_frame_data.insert(full_frame_data.end(), bson_header_part.begin(), bson_header_part.end());
        full_frame_data.insert(full_frame_data.end(), payload_buffer.begin(), payload_buffer.end());

        BOOST_LOG_SEV(lg, debug) << "Reconstructed full frame data size: " << full_frame_data.size();
        
        // Now deserialize the complete frame
        auto frame_result = protocol::frame::deserialize(full_frame_data);
        if (!frame_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize complete frame, error code: " << static_cast<int>(frame_result.error());
        } else {
            BOOST_LOG_SEV(lg, debug) << "Successfully deserialized complete frame, type: " << static_cast<int>(frame_result->header().type);
        }
        
        co_return frame_result;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Exception in read_frame: " << e.what();
        co_return std::unexpected(protocol::error_code::invalid_message_type);
    }
}

cobalt::promise<void> connection::write_frame(const protocol::frame& frame) {
    auto data = frame.serialize();
    BOOST_LOG_SEV(lg, debug) << "Writing frame of size " << data.size() 
                              << ", type: " << static_cast<int>(frame.header().type) 
                              << ", sequence: " << frame.header().sequence;
    co_await boost::asio::async_write(
        socket_,
        boost::asio::buffer(data),
        cobalt::use_op);
    BOOST_LOG_SEV(lg, debug) << "Successfully wrote frame";
}

bool connection::is_open() const {
    return socket_.lowest_layer().is_open();
}

void connection::close() {
    if (is_open()) {
        boost::system::error_code ec;
        socket_.lowest_layer().close(ec);
    }
}

std::string connection::remote_address() const {
    try {
        auto endpoint = socket_.lowest_layer().remote_endpoint();
        return endpoint.address().to_string() + ":" + std::to_string(endpoint.port());
    } catch (const std::exception&) {
        return "unknown";
    }
}

}
