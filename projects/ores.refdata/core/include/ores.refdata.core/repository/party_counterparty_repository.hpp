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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_REPOSITORY_PARTY_COUNTERPARTY_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_PARTY_COUNTERPARTY_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_counterparty.hpp"
#include "ores.refdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes party counterparties to data storage.
 */
class ORES_REFDATA_CORE_EXPORT party_counterparty_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_counterparty_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit party_counterparty_repository(context ctx);

    std::string sql();

    void write(const domain::party_counterparty& party_counterparty);
    void write(const std::vector<domain::party_counterparty>& party_counterparties);

    std::vector<domain::party_counterparty> read_latest();
    std::vector<domain::party_counterparty> read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparties.
     */
    std::uint32_t get_total_party_counterparty_count();
    std::vector<domain::party_counterparty>
    read_latest_by_party(const boost::uuids::uuid& party_id);
    /**
     * @brief Reads latest party counterparties filtered by party_id, with pagination.
     */
    std::vector<domain::party_counterparty> read_latest_by_party(const boost::uuids::uuid& party_id,
                                                                 std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active party counterparties filtered by party_id.
     */
    std::uint32_t get_total_party_counterparty_count_by_party(const boost::uuids::uuid& party_id);

    std::vector<domain::party_counterparty>
    read_latest_by_counterparty(const boost::uuids::uuid& counterparty_id);

    /**
     * @brief Gets the total count of active party counterparties filtered by counterparty_id.
     */
    std::uint32_t
    get_total_party_counterparty_count_by_counterparty(const boost::uuids::uuid& counterparty_id);

    void remove(const boost::uuids::uuid& party_id, const boost::uuids::uuid& counterparty_id);
    void remove_by_party(const boost::uuids::uuid& party_id);

private:
    context ctx_;
};

}

#endif
