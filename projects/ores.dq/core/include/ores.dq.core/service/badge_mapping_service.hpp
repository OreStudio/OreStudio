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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_SERVICE_BADGE_MAPPING_SERVICE_HPP
#define ORES_DQ_SERVICE_BADGE_MAPPING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_mapping.hpp"
#include "ores.dq.api/messaging/badge_mapping_protocol.hpp"
#include "ores.dq.core/repository/badge_mapping_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge mappings.
 *
 * Provides a higher-level interface for badge mapping operations,
 * wrapping the underlying repository.
 */
class badge_mapping_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_mapping_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_mapping_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit badge_mapping_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_badge_mappings_response
    list_badge_mappings(const messaging::list_badge_mappings_request& request);
    messaging::get_badge_mapping_response
    get_badge_mapping(const messaging::get_badge_mapping_request& request);
    messaging::get_many_badge_mappings_response
    get_many_badge_mappings(const messaging::get_many_badge_mappings_request& request);
    messaging::put_badge_mapping_response
    put_badge_mapping(const messaging::put_badge_mapping_request& request);
    messaging::put_many_badge_mappings_response
    put_many_badge_mappings(const messaging::put_many_badge_mappings_request& request);
    messaging::delete_badge_mapping_response
    delete_badge_mapping(const messaging::delete_badge_mapping_request& request);
    messaging::delete_many_badge_mappings_response
    delete_many_badge_mappings(const messaging::delete_many_badge_mappings_request& request);
    messaging::list_by_code_domain_code_badge_mappings_response
    list_by_code_domain_code_badge_mappings(
        const messaging::list_by_code_domain_code_badge_mappings_request& request);
    /**@}*/

private:
    context ctx_;
    repository::badge_mapping_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::badge_mapping_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::badge_mapping& out);
};

}

#endif
