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
#ifndef ORES_REFDATA_SERVICE_PARTY_CURRENCY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_CURRENCY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_currency.hpp"
#include "ores.refdata.core/repository/party_currency_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party currencies.
 *
 * This service provides functionality for:
 * - Managing party currencies (CRUD operations)
 */
class party_currency_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_currency_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_currency_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_currency_service(context ctx);

    /**
     * @brief Lists all party currencies.
     */
    std::vector<domain::party_currency> list_party_currencies();

    /**
     * @brief Lists party currencies with pagination.
     */
    std::vector<domain::party_currency> list_party_currencies(std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active party currencies.
     */
    std::uint32_t get_total_party_currency_count();

    /**
     * @brief Lists party currencies for a specific party.
     *
     * @param party_id The party to filter by
     */
    std::vector<domain::party_currency>
    list_party_currencies_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Lists party currencies for a specific party, with pagination.
     */
    std::vector<domain::party_currency> list_party_currencies_by_party(
        const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party currencies filtered by party_id.
     */
    std::uint32_t get_total_party_currency_count_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Gets the total count of active party currencies filtered by currency_iso_code.
     */
    std::uint32_t get_total_party_currency_count_by_currency(const std::string& currency_iso_code);
    /**
     * @brief Saves a party currency (creates or updates).
     *
     * @param party_currency The party currency to save
     */
    void save_party_currency(const domain::party_currency& party_currency);

    /**
     * @brief Saves a batch of party currencies in one transaction.
     *
     * @param party_currencies The party currencies to save
     */
    void save_party_currencies(const std::vector<domain::party_currency>& party_currencies);

    /**
     * @brief Removes a party currency.
     *
     * @param party_id The party
     * @param currency_iso_code The currency
     */
    void remove_party_currency(const boost::uuids::uuid& party_id,
                               const std::string& currency_iso_code);


private:
    context ctx_;
    repository::party_currency_repository repo_;
};

}

#endif
