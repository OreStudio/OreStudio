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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
// Measures four ways of getting one ORE XML document into ORE Studio records.
//
//   1  xsdcpp: generated DOM, then the hand-written mapper
//   2  pugixml: heap DOM, then a table walk
//   3  this codec: one pass into records that own their strings
//   4  this codec: one pass into records that borrow the document
//   5  scanner only: events, no records, to separate parse from conversion
//   6  round trip: read the document, write it back, read it again
//   7  exact numbers: every numeric leaf whose text a double cannot reproduce
//
// Variants 2, 3 and 4 all end in the same descriptor table, so the difference
// between them is the document model and the record layout, not the mapping.

#include "alloc_counter.hpp"
#include "codec.hpp"

#include "ores.ore.core/domain/currency_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"

#include <pugixml.hpp>

#include <algorithm>
#include <charconv>
#include <cstdlib>
#include <chrono>
#include <cstdio>
#include <fstream>
#include <string>
#include <vector>
#include <iterator>

namespace {

using clock_type = std::chrono::steady_clock;

constexpr const char* kRoundingSpellings[] = {"Up", "Down", "Closest", "Floor", "Ceiling"};

auto slurp(const char* path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

auto elapsed_ms(clock_type::time_point from) -> double {
    return std::chrono::duration<double, std::milli>(clock_type::now() - from).count();
}

// A normalised decimal: sign, significant digits with leading and trailing
// zeros removed, and the power of ten the first digit stands for. Two
// spellings of the same number normalise to the same triple, whatever the
// notation, so "0.000054" and "5.4e-05" agree while a truncated double does
// not.
struct decimal_parts {
    bool negative = false;
    std::string digits;
    int exponent = 0;

    friend auto operator==(const decimal_parts&, const decimal_parts&) -> bool = default;
};

auto canonical(std::string_view text) -> decimal_parts {
    decimal_parts parts;
    std::size_t i = 0;
    if (i < text.size() && (text[i] == '+' || text[i] == '-'))
        parts.negative = text[i++] == '-';

    std::string digits;
    int exponent = 0;
    bool after_point = false;
    for (; i < text.size(); ++i) {
        const char c = text[i];
        if (c == '.') {
            after_point = true;
        } else if (c >= '0' && c <= '9') {
            digits.push_back(c);
            if (after_point)
                --exponent;
        } else {
            break;
        }
    }
    if (i < text.size() && (text[i] == 'e' || text[i] == 'E'))
        exponent += std::atoi(std::string(text.substr(i + 1)).c_str());

    std::size_t lead = 0;
    while (lead < digits.size() && digits[lead] == '0')
        ++lead;
    digits.erase(0, lead);
    while (!digits.empty() && digits.back() == '0') {
        digits.pop_back();
        ++exponent;
    }
    if (digits.empty()) {
        parts.negative = false;
        parts.exponent = 0;
        return parts;
    }
    parts.digits = std::move(digits);
    parts.exponent = exponent + 1;
    return parts;
}

auto is_single_number(std::string_view text) -> bool {
    return text.find(',') == std::string_view::npos && text.find(' ') == std::string_view::npos;
}

auto has(const ore::xml::currency_row& row, std::string_view name) -> bool {
    const auto* f = ore::xml::detail::find_field(*ore::xml::currency_document().item, name);
    return f != nullptr &&
           ore::xml::detail::is_present(reinterpret_cast<const std::byte*>(&row), *f);
}

// One line per record, so the four variants can be compared as text.
auto render(const ore::xml::currency_row& r) -> std::string {
    std::string out;
    out += r.iso_code;
    out += '|';
    out += r.name;
    out += '|';
    out += has(r, "NumericCode") ? std::to_string(r.numeric_code) : std::string{};
    out += '|';
    out += r.symbol;
    out += '|';
    out += r.fraction_symbol;
    out += '|';
    out += std::to_string(r.fractions_per_unit);
    out += '|';
    out += kRoundingSpellings[r.rounding_type];
    out += '|';
    out += std::to_string(r.rounding_precision);
    out += '|';
    out += has(r, "CurrencyType") ? r.currency_type : std::string{};
    return out;
}

auto render(const ores::refdata::domain::currency& c) -> std::string {
    std::string out;
    out += c.iso_code;
    out += '|';
    out += c.name;
    out += '|';
    out += c.numeric_code;
    out += '|';
    out += c.symbol;
    out += '|';
    out += c.fraction_symbol;
    out += '|';
    out += std::to_string(c.fractions_per_unit);
    out += '|';
    out += c.rounding_type;
    out += '|';
    out += std::to_string(c.rounding_precision);
    out += '|';
    out += c.ore_currency_type.value_or("");
    return out;
}

} // namespace

int main(int argc, char** argv) {
    if (argc < 3) {
        std::printf("usage: bench <variant 1-5> <document>\n");
        return 1;
    }
    const int variant = std::atoi(argv[1]);
    const char* path = argv[2];
    const auto document = slurp(path);
    std::printf("document   : %s (%zu bytes)\n", path, document.size());

    if (variant == 8) {
        double best = 1e18;
        for (int i = 0; i < 5; ++i) {
            const auto from = clock_type::now();
            pugi::xml_document tree;
            tree.load_buffer(document.data(), document.size());
            (void)std::distance(tree.begin(), tree.end());
            best = std::min(best, elapsed_ms(from));
        }
        std::printf("pugixml    : %.2f ms  %.0f MB/s\n",
                    best,
                    static_cast<double>(document.size()) / best / 1000.0);
        return 0;
    }

    if (variant == 7) {
        ore::xml::reader input(document);
        std::size_t numeric = 0;
        std::size_t inexact = 0;
        std::size_t reformatted = 0;
        std::size_t single_precision = 0;
        std::size_t shown = 0;
        for (;;) {
            const auto ev = input.next();
            if (ev.kind == ore::xml::event_type::end_of_document)
                break;
            if (ev.kind == ore::xml::event_type::error) {
                std::printf("error: %s at %u:%u\n",
                            std::string(input.error()).c_str(),
                            input.line(),
                            input.column());
                return 2;
            }
            if (ev.kind != ore::xml::event_type::text ||
                ev.value.find('.') == std::string_view::npos || !is_single_number(ev.value))
                continue;
            numeric += 1;
            const std::string text(ev.value);
            char* end = nullptr;
            const double value = std::strtod(text.c_str(), &end);
            char shortest[64];
            const auto written = std::to_chars(shortest, shortest + sizeof(shortest), value);
            const std::string_view as_double(shortest, written.ptr);
            char shortest_float[64];
            const auto as_float = std::to_chars(shortest_float,
                                                shortest_float + sizeof(shortest_float),
                                                static_cast<float>(value));
            if (canonical(std::string_view(shortest_float, as_float.ptr)) != canonical(text))
                single_precision += 1;

            if (canonical(as_double) != canonical(text)) {
                inexact += 1;
                if (shown < 6) {
                    ++shown;
                    std::printf("  digits lost  %-28s -> %.*s\n",
                            text.c_str(),
                            static_cast<int>(as_double.size()),
                            as_double.data());
                }
            } else if (as_double != text) {
                reformatted += 1;
                if (shown < 6) {
                    ++shown;
                    std::printf("  form only    %-28s -> %.*s\n",
                            text.c_str(),
                            static_cast<int>(as_double.size()),
                            as_double.data());
                }
            }
        }
        std::printf("numeric    : %zu\n", numeric);
        std::printf("digits lost: %zu (%.1f%%)\n",
                    inexact,
                    numeric == 0 ? 0.0 : 100.0 * static_cast<double>(inexact) / numeric);
        std::printf("float loss : %zu (%.1f%%)\n",
                    single_precision,
                    numeric == 0 ? 0.0 : 100.0 * static_cast<double>(single_precision) / numeric);
        std::printf("form lost  : %zu (%.1f%%)\n",
                    reformatted,
                    numeric == 0 ? 0.0 : 100.0 * static_cast<double>(reformatted) / numeric);
        return 0;
    }

    if (variant == 6) {
        std::vector<ore::xml::currency_row> first;
        const auto read = ore::xml::read_currencies(
            document, [&](const ore::xml::currency_row& row) { first.push_back(row); });
        if (!read.ok) {
            std::printf("error: %s at %u:%u\n",
                        std::string(read.message).c_str(),
                        read.line,
                        read.column);
            return 2;
        }
        std::string written;
        ore::xml::write_currencies(first, written);
        std::vector<ore::xml::currency_row> second;
        const auto reread = ore::xml::read_currencies(
            written, [&](const ore::xml::currency_row& row) { second.push_back(row); });
        if (!reread.ok) {
            std::printf("error: %s at %u:%u\n",
                        std::string(reread.message).c_str(),
                        reread.line,
                        reread.column);
            return 2;
        }
        bool identical = first.size() == second.size();
        for (std::size_t i = 0; identical && i < first.size(); ++i)
            identical = render(first[i]) == render(second[i]);
        std::printf("rows       : %zu\n", first.size());
        std::printf("written    : %zu bytes\n", written.size());
        std::printf("round trip : %s\n", identical ? "identical" : "DIFFERENT");
        return identical ? 0 : 3;
    }

    if (variant == 5) {
        double best = 1e18;
        std::size_t events = 0;
        for (int i = 0; i < 7; ++i) {
            std::size_t count = 0;
            const auto from = clock_type::now();
            ore::xml::reader input(document);
            for (;;) {
                const auto ev = input.next();
                if (ev.kind == ore::xml::event_type::end_of_document)
                    break;
                if (ev.kind == ore::xml::event_type::error) {
                    std::printf("error: %s at %u:%u\n",
                                std::string(input.error()).c_str(),
                                input.line(),
                                input.column());
                    return 2;
                }
                ++count;
            }
            best = std::min(best, elapsed_ms(from));
            events = count;
        }
        std::printf("scanner    : %.2f ms  %.0f MB/s  %zu events\n",
                    best,
                    static_cast<double>(document.size()) / best / 1000.0,
                    events);
        return 0;
    }

    double best = 1e18;
    std::size_t records = 0;
    unsigned long long allocations = 0;
    unsigned long long bytes = 0;
    std::vector<std::string> reference;
    std::vector<std::string> rendered;

    for (int repeat = 0; repeat < 7; ++repeat) {
        std::vector<std::string> rows;
        std::size_t read_count = 0;
        bench::scope counter;
        const auto from = clock_type::now();

        switch (variant) {
            case 1: {
                ores::ore::domain::currencyConfig config;
                ores::ore::domain::load_data(document, config);
                for (const auto& mapped : ores::ore::domain::currency_mapper::map(config))
                    rows.push_back(render(mapped));
                break;
            }
            case 2: {
                pugi::xml_document tree;
                if (!tree.load_buffer(document.data(), document.size())) {
                    std::printf("pugixml rejected the document\n");
                    return 2;
                }
                for (const auto& currency : tree.child("CurrencyConfig").children("Currency")) {
                    ore::xml::currency_row row;
                    for (const auto& child : currency.children()) {
                        const auto* f = ore::xml::detail::find_field(
                            *ore::xml::currency_document().item, child.name());
                        if (f == nullptr)
                            continue;
                        const auto stored =
                            ore::xml::detail::store_value<ore::xml::detail::materialized>(
                                *f,
                                reinterpret_cast<std::byte*>(&row),
                                std::string_view(child.text().get()));
                        if (!stored.ok)
                            return 2;
                    }
                    rows.push_back(render(row));
                }
                break;
            }
            case 3: {
                std::vector<ore::xml::currency_row> parsed;
                const auto read = ore::xml::read_currencies(
                    document, [&](const ore::xml::currency_row& row) { parsed.push_back(row); });
                if (!read.ok) {
                    std::printf("error: %s at %u:%u\n",
                                std::string(read.message).c_str(),
                                read.line,
                                read.column);
                    return 2;
                }
                for (const auto& row : parsed)
                    rows.push_back(render(row));
                break;
            }
            case 4: {
                std::size_t checksum = 0;
                read_count = 0;
                const auto read = ore::xml::read_currency_views(
                    document, [&](const ore::xml::currency_view& row) {
                        checksum += row.iso_code.size() + row.name.size() +
                                    row.fraction_symbol.size() + row.currency_type.size();
                        checksum += static_cast<std::size_t>(row.rounding_type);
                        ++read_count;
                    });
                if (!read.ok) {
                    std::printf("error: %s at %u:%u\n",
                                std::string(read.message).c_str(),
                                read.line,
                                read.column);
                    return 2;
                }
                rows.assign(1, std::to_string(checksum));
                break;
            }
            default:
                std::printf("unknown variant\n");
                return 1;
        }

        records = variant == 4 ? read_count : rows.size();
        const auto ms = elapsed_ms(from);
        allocations = counter.allocs();
        bytes = counter.bytes();
        if (repeat == 0)
            reference = rows;
        else if (rows != reference) {
            std::printf("variant %d is not deterministic\n", variant);
            return 3;
        }
        rendered = std::move(rows);
        best = std::min(best, ms);
    }

    std::printf("rows       : %zu\n", records);
    std::printf("best       : %.3f ms\n", best);
    std::printf("allocations: %llu (%llu bytes)\n", allocations, bytes);
    if (argc > 3 && std::string_view(argv[3]) == "--dump")
        for (const auto& line : rendered)
            std::printf("%s\n", line.c_str());
    else
        std::printf("sample     : %s\n", rendered.empty() ? "-" : rendered.front().c_str());
    return 0;
}
