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
#ifndef ORES_REFDATA_SERVICE_PARTY_COUNTERPARTY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_COUNTERPARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_counterparty.hpp"
#include "ores.refdata.core/repository/party_counterparty_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party counterparties.
 *
 * This service provides functionality for:
 * - Managing party counterparties (CRUD operations)
 */
class party_counterparty_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_counterparty_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_counterparty_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_counterparty_service(context ctx);

    /**
     * @brief Lists all party counterparties.
     */
    std::vector<domain::party_counterparty> list_party_counterparties();

    /**
     * @brief Lists party counterparties with pagination.
     */
    std::vector<domain::party_counterparty> list_party_counterparties(std::uint32_t offset,
                                                                      std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparties.
     */
    std::uint32_t get_total_party_counterparty_count();

    /**
     * @brief Lists party counterparties for a specific party.
     *
     * @param party_id The party to filter by
     */
    std::vector<domain::party_counterparty>
    list_party_counterparties_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Lists party counterparties for a specific party, with pagination.
     */
    std::vector<domain::party_counterparty> list_party_counterparties_by_party(
        const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparties filtered by party_id.
     */
    std::uint32_t get_total_party_counterparty_count_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Gets the total count of active party counterparties filtered by counterparty_id.
     */
    std::uint32_t
    get_total_party_counterparty_count_by_counterparty(const boost::uuids::uuid& counterparty_id);
    /**
     * @brief Saves a party counterparty (creates or updates).
     *
     * @param party_counterparty The party counterparty to save
     */
    void save_party_counterparty(const domain::party_counterparty& party_counterparty);

    /**
     * @brief Saves a batch of party counterparties in one transaction.
     *
     * @param party_counterparties The party counterparties to save
     */
    void
    save_party_counterparties(const std::vector<domain::party_counterparty>& party_counterparties);

    /**
     * @brief Removes a party counterparty.
     *
     * @param party_id The party
     * @param counterparty_id The counterparty
     */
    void remove_party_counterparty(const boost::uuids::uuid& party_id,
                                   const boost::uuids::uuid& counterparty_id);


private:
    context ctx_;
    repository::party_counterparty_repository repo_;
};

}

#endif
