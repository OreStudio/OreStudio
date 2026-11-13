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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_API_MESSAGING_RUN_FILES_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_RUN_FILES_PROTOCOL_HPP

#include "ores.compute.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::compute::messaging {

/**
 * @brief One package a run read or wrote.
 */
struct run_file_archive {
    /**
     * @brief "input" for a package the engine read, "output" for one it wrote.
     */
    std::string direction = "";
    /**
     * @brief The workunit whose package this is.
     */
    std::string workunit_id = "";
    /**
     * @brief The result that wrote an output package, empty for an input.
     */
    std::string result_id = "";
    /**
     * @brief The object path the package is stored at.
     *
     * A path and not a bucket and a key, because that is what the workunit row
     * states and what a caller hands back to storage without rebuilding it.
     */
    std::string uri = "";

    friend bool operator==(const run_file_archive&, const run_file_archive&) = default;
};

/**
 * @brief Asks for the packages one report run read and wrote.
 */
struct list_run_files_request {
    using response_type = struct list_run_files_response;
    static constexpr std::string_view nats_subject = "compute.v1.ops.list_run_files";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The run the caller is showing. It is the batch's external reference.
     */
    std::string run_id;

    friend bool operator==(const list_run_files_request&, const list_run_files_request&) = default;
};

/**
 * @brief The run's packages, in the order the workunits were dispatched.
 */
struct list_run_files_response {
    bool success = true;
    std::string message = "";
    std::string run_id = "";
    /**
     * @brief The batch the run drove, empty when the run has none yet.
     */
    std::string batch_id = "";
    std::vector<run_file_archive> archives;

    friend bool operator==(const list_run_files_response&,
                           const list_run_files_response&) = default;
};

/**
 * @brief The members of run_file_archive, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_file_archive>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"direction", &run_file_archive::direction},
        ::ores::utility::codec::field{"workunit_id", &run_file_archive::workunit_id},
        ::ores::utility::codec::field{"result_id", &run_file_archive::result_id},
        ::ores::utility::codec::field{"uri", &run_file_archive::uri},
    };
}

/**
 * @brief The members of list_run_files_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_files_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"run_id", &list_run_files_request::run_id},
    };
}

/**
 * @brief The members of list_run_files_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_files_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_run_files_response::success},
        ::ores::utility::codec::field{"message", &list_run_files_response::message},
        ::ores::utility::codec::field{"run_id", &list_run_files_response::run_id},
        ::ores::utility::codec::field{"batch_id", &list_run_files_response::batch_id},
        ::ores::utility::codec::field{"archives", &list_run_files_response::archives},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::run_file_archive> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::run_file_archive& v);
    static ores::compute::messaging::run_file_archive read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::run_file_archive& v);
    static ores::compute::messaging::run_file_archive read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_run_files_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_run_files_request& v);
    static ores::compute::messaging::list_run_files_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_run_files_request& v);
    static ores::compute::messaging::list_run_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_run_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_run_files_response& v);
    static ores::compute::messaging::list_run_files_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_run_files_response& v);
    static ores::compute::messaging::list_run_files_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
