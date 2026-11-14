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
#ifndef ORES_IAM_SERVICE_ROLE_PRODUCT_TYPE_SERVICE_HPP
#define ORES_IAM_SERVICE_ROLE_PRODUCT_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/role_product_type.hpp"
#include "ores.iam.api/messaging/role_product_type_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/role_product_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing permitted product types.
 *
 * Provides a higher-level interface for permitted product type operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT role_product_type_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.role_product_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a role_product_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit role_product_type_service(const context& ctx);

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
    ::ores::iam::messaging::list_role_product_types_response
    list_role_product_types(const ::ores::iam::messaging::list_role_product_types_request& request);
    ::ores::iam::messaging::get_role_product_type_response
    get_role_product_type(const ::ores::iam::messaging::get_role_product_type_request& request);
    ::ores::iam::messaging::get_many_role_product_types_response get_many_role_product_types(
        const ::ores::iam::messaging::get_many_role_product_types_request& request);
    ::ores::iam::messaging::put_role_product_type_response
    put_role_product_type(const ::ores::iam::messaging::put_role_product_type_request& request);
    ::ores::iam::messaging::put_many_role_product_types_response put_many_role_product_types(
        const ::ores::iam::messaging::put_many_role_product_types_request& request);
    ::ores::iam::messaging::delete_role_product_type_response delete_role_product_type(
        const ::ores::iam::messaging::delete_role_product_type_request& request);
    ::ores::iam::messaging::delete_many_role_product_types_response delete_many_role_product_types(
        const ::ores::iam::messaging::delete_many_role_product_types_request& request);
    ::ores::iam::messaging::list_by_role_id_role_product_types_response
    list_by_role_id_role_product_types(
        const ::ores::iam::messaging::list_by_role_id_role_product_types_request& request);
    ::ores::iam::messaging::list_by_product_type_code_role_product_types_response
    list_by_product_type_code_role_product_types(
        const ::ores::iam::messaging::list_by_product_type_code_role_product_types_request&
            request);
    /**@}*/

private:
    context ctx_;
    repository::role_product_type_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::iam::messaging::role_product_type_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::role_product_type& out);
};

}

#endif
