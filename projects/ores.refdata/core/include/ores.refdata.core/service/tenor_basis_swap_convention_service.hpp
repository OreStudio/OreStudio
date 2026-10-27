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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_BASIS_SWAP_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_BASIS_SWAP_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_convention_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for the composite tenor basis swap convention family.
 *
 * The family is one central row plus its parts. A read answers the central
 * row and every part live at the same instant; a write lands the central row
 * and reconciles each part in one transaction, so no reader sees the parts
 * disagree with the central version. See the "The atomised entity" and
 * "Temporal composite entity versioning" architecture documents.
 */
class ORES_REFDATA_CORE_EXPORT tenor_basis_swap_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.tenor_basis_swap_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_basis_swap_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_basis_swap_convention_service(context ctx);

    /**
     * @brief The family's protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything.
     */
    /**@{*/
    ::ores::refdata::messaging::get_tenor_basis_swap_convention_response
    get_tenor_basis_swap_convention(
        const ::ores::refdata::messaging::get_tenor_basis_swap_convention_request& request);
    ::ores::refdata::messaging::get_many_tenor_basis_swap_conventions_response
    get_many_tenor_basis_swap_conventions(
        const ::ores::refdata::messaging::get_many_tenor_basis_swap_conventions_request& request);
    ::ores::refdata::messaging::put_tenor_basis_swap_convention_response
    put_tenor_basis_swap_convention(
        const ::ores::refdata::messaging::put_tenor_basis_swap_convention_request& request);
    ::ores::refdata::messaging::put_many_tenor_basis_swap_conventions_response
    put_many_tenor_basis_swap_conventions(
        const ::ores::refdata::messaging::put_many_tenor_basis_swap_conventions_request& request);
    /**@}*/

private:
    context ctx_;
    repository::convention_repository central_repo_;
    repository::tenor_basis_swap_pay_receive_repository pay_receive_repo_;
    repository::tenor_basis_swap_long_short_repository long_short_repo_;

    /**
     * @brief Assembles the family item: the central row and every part live
     * at the same instant.
     */
    ::ores::refdata::messaging::tenor_basis_swap_convention_item
    assemble(const context& ctx,
             const ::ores::refdata::messaging::tenor_basis_swap_convention_key& key,
             const std::optional<domain::convention>& central);

    /**
     * @brief Checks one central change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check and the
     * stamp are one decision made in one place.
     */
    ores::utility::domain::result
    prepare_convention(const ::ores::refdata::messaging::convention_change& change,
                       const ores::utility::domain::change_intent& intent,
                       const context& ctx,
                       domain::convention& out);

    /**
     * @brief Checks one part change against the row it names, and stamps it.
     */
    ores::utility::domain::result prepare_pay_receive(
        const ::ores::refdata::messaging::tenor_basis_swap_pay_receive_change& change,
        const ores::utility::domain::change_intent& intent,
        const context& ctx,
        domain::tenor_basis_swap_pay_receive& out);

    /**
     * @brief Makes this part's rows for one central row exactly the rows the
     * request carries.
     *
     * The rows present are written and the rows currently live but absent are
     * closed. The part table's insert trigger bumps the central row's version,
     * so this method never does.
     */
    ores::utility::domain::result reconcile_pay_receive(
        const context& ctx,
        const domain::convention& central,
        const std::vector<::ores::refdata::messaging::tenor_basis_swap_pay_receive_change>&
            requested,
        const ores::utility::domain::change_intent& intent);

    /**
     * @brief Checks one part change against the row it names, and stamps it.
     */
    ores::utility::domain::result
    prepare_long_short(const ::ores::refdata::messaging::tenor_basis_swap_long_short_change& change,
                       const ores::utility::domain::change_intent& intent,
                       const context& ctx,
                       domain::tenor_basis_swap_long_short& out);

    /**
     * @brief Makes this part's rows for one central row exactly the rows the
     * request carries.
     *
     * The rows present are written and the rows currently live but absent are
     * closed. The part table's insert trigger bumps the central row's version,
     * so this method never does.
     */
    ores::utility::domain::result reconcile_long_short(
        const context& ctx,
        const domain::convention& central,
        const std::vector<::ores::refdata::messaging::tenor_basis_swap_long_short_change>&
            requested,
        const ores::utility::domain::change_intent& intent);
};

}

#endif
