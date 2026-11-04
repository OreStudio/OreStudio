/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#ifndef ORES_TRADING_CORE_SERVICE_BUS_BOOK_RULES_HPP
#define ORES_TRADING_CORE_SERVICE_BUS_BOOK_RULES_HPP

#include "ores.nats/service/client.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/service/book_rules_port.hpp"
#include <string>

namespace ores::trading::service {

/** The subject that answers a book's own row. */
inline constexpr std::string_view refdata_books_subject = "refdata.v1.books.list";

/** The subject that answers the product types a book allows. */
inline constexpr std::string_view refdata_book_product_types_subject =
    "refdata.v1.book_product_types.list";

/**
 * @brief A book's rules, asked of the service that owns them, as the caller.
 *
 * The read is forwarded with the caller's own token in
 * `X-Delegated-Authorization`, so refdata answers for the person booking and a
 * book they cannot see comes back `unknown` rather than open. Every failure
 * answers `unknown`: a booking must not be refused for a fault in the asking.
 *
 * The status and the flag live on the book, so one read answers both. The
 * junction is read only when the book restricts its product types, which is the
 * reading the flag exists to spare.
 */
class ORES_TRADING_CORE_EXPORT bus_book_rules final : public book_rules_port {
public:
    explicit bus_book_rules(
        ores::nats::service::client& nats,
        std::string books_subject = std::string(refdata_books_subject),
        std::string product_types_subject = std::string(refdata_book_product_types_subject))
        : nats_(nats)
        , books_subject_(std::move(books_subject))
        , product_types_subject_(std::move(product_types_subject)) {}

    [[nodiscard]] answer of(const boost::uuids::uuid& book_id,
                            std::string_view product_type_code,
                            std::string_view actor_token) const override;

private:
    ores::nats::service::client& nats_;
    std::string books_subject_;
    std::string product_types_subject_;
};

}

#endif
