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
#include "ores.http/routes/reporting/cut_kind_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/cut_kind_rows.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ores.reporting.api/messaging/cut_kind_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <cctype>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace refdata_messaging = ores::refdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
using ores::utility::codec::field;

/// The largest page any read here asks for: a kind's lookup is a closed set.
constexpr std::uint32_t kinds_limit = 1000;

/// How many cuts the list counts its kinds from, which is the list's own page.
constexpr std::uint32_t kind_counts_read = 1000;

/// How many of a kind's cuts its record answers before it says there are more.
constexpr std::uint32_t cuts_of_a_kind_page = 500;

namespace kinds {

/// The shapes a page of kinds answers, refused by their sentences.
struct kind_page final {
    std::uint32_t offset = 0;
    std::uint32_t limit = 0;
    std::string search;
    std::string sort;
    bool descending = false;
};

/// The fields the kinds list orders by, and nothing else.
bool is_kind_sort(const std::string& sort) {
    return sort.empty() || sort == "code" || sort == "name" || sort == "family";
}

std::string lowered(std::string text) {
    std::ranges::transform(text, text.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return text;
}

kind_page kind_page_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"offset", "limit", "search", "sort", "descending"});
    kind_page out;
    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        try {
            out.offset = static_cast<std::uint32_t>(std::stoul(raw));
        } catch (const std::exception&) {
            rr::invalid("A page names an offset and a limit of 1 to 1000.");
        }
    }
    const auto raw_limit = req.get_query_param("limit");
    std::uint32_t stated = 0;
    if (!raw_limit.empty()) {
        try {
            stated = static_cast<std::uint32_t>(std::stoul(raw_limit));
        } catch (const std::exception&) {
            rr::invalid("A page names an offset and a limit of 1 to 1000.");
        }
    }
    if (stated < 1 || stated > 1000)
        rr::invalid("A page names an offset and a limit of 1 to 1000.");
    out.limit = stated;
    out.search = rr::trimmed(req.get_query_param("search"));
    out.sort = req.get_query_param("sort");
    if (!is_kind_sort(out.sort))
        rr::invalid("The kinds of cut cannot be ordered by " + out.sort + ".");
    out.descending = req.get_query_param("descending") == "true";
    return out;
}

/// One kind's write, as the form states it.
struct kind_change final {
    std::string name;
    std::string description;
    std::string family_code;
    std::optional<std::string> approval_kind_code;
    bool output_is_indicative = false;
    std::string resolution_rule_code;
    int retention_months = 0;
    bool legal_hold_default = false;
    int display_order = 0;
};

constexpr auto wire_fields(std::type_identity<kind_change>) {
    return ores::utility::codec::field_list{
        field{"name", &kind_change::name},
        field{"description", &kind_change::description},
        field{"familyCode", &kind_change::family_code},
        field{"approvalKindCode", &kind_change::approval_kind_code},
        field{"outputIsIndicative", &kind_change::output_is_indicative},
        field{"resolutionRuleCode", &kind_change::resolution_rule_code},
        field{"retentionMonths", &kind_change::retention_months},
        field{"legalHoldDefault", &kind_change::legal_hold_default},
        field{"displayOrder", &kind_change::display_order}};
}

/// The body a kind's form sends: the version it read, the change, and why.
struct kind_edit_body final {
    std::uint32_t version = 0;
    kind_change change;
    rr::stated_intent intent;
};

constexpr auto wire_fields(std::type_identity<kind_edit_body>) {
    return ores::utility::codec::field_list{field{"version", &kind_edit_body::version},
                                            field{"change", &kind_edit_body::change},
                                            field{"intent", &kind_edit_body::intent}};
}

/// The body a removal sends.
struct kind_removal_body final {
    std::uint32_t version = 0;
    rr::stated_intent intent;
};

constexpr auto wire_fields(std::type_identity<kind_removal_body>) {
    return ores::utility::codec::field_list{field{"version", &kind_removal_body::version},
                                            field{"intent", &kind_removal_body::intent}};
}

/// One kind as the store holds it, by code, or nothing.
std::optional<ores::reporting::domain::cut_kind>
domain_kind_of(const http_request& req, nats_client& session, const std::string& code) {
    reporting_messaging::get_cut_kind_request msg;
    msg.key.code = code;
    const auto reply = forwarding::forward(req, session, msg);
    if (!reply.cut_kind)
        return std::nullopt;
    return *reply.cut_kind;
}

/// The cuts of the party, read once so a page of kinds costs one cut read.
struct cut_counts final {
    std::map<std::string, int> per_kind;
    int total = 0;
    bool partial = false;
};

cut_counts kind_cut_counts_of(const http_request& req, nats_client& session) {
    reporting_messaging::list_cut_rows_request msg;
    msg.limit = kind_counts_read;
    const auto reply = forwarding::forward(req, session, msg);
    cut_counts out;
    out.total = reply.total;
    for (const auto& row : reply.rows)
        ++out.per_kind[row.kind_code];
    out.partial = reply.total > static_cast<int>(reply.rows.size());
    return out;
}

/// The kinds the store holds, each with the words its three codes stand for.
std::vector<cuts::kind_row> all_kinds(const http_request& req, nats_client& session) {
    const auto lookups = cuts::read_kind_lookups(req, session);
    const auto counts = kind_cut_counts_of(req, session);

    reporting_messaging::list_cut_kinds_request msg;
    msg.limit = kinds_limit;
    const auto reply = forwarding::forward(req, session, msg);

    std::vector<cuts::kind_row> rows;
    rows.reserve(reply.kinds.size());
    for (const auto& kind : reply.kinds) {
        const auto found = counts.per_kind.find(kind.code);
        rows.push_back(cuts::row_of(kind,
                                    lookups,
                                    found == counts.per_kind.end() ? 0 : found->second));
    }
    std::ranges::sort(rows, [](const cuts::kind_row& left, const cuts::kind_row& right) {
        if (left.display_order != right.display_order)
            return left.display_order < right.display_order;
        return left.code < right.code;
    });
    return rows;
}

/// The kinds a search matched, largest of nothing and in the order asked for.
std::vector<cuts::kind_row> kinds_matched(const std::vector<cuts::kind_row>& rows,
                                         const kind_page& asked) {
    const auto needle = lowered(asked.search);
    std::vector<cuts::kind_row> found;
    for (const auto& row : rows) {
        if (needle.empty() || lowered(row.code).find(needle) != std::string::npos ||
            lowered(row.name).find(needle) != std::string::npos ||
            lowered(row.family_name).find(needle) != std::string::npos)
            found.push_back(row);
    }
    if (asked.sort == "code") {
        std::ranges::sort(found, [](const auto& left, const auto& right) {
            return left.code < right.code;
        });
    } else if (asked.sort == "name") {
        std::ranges::sort(found, [](const auto& left, const auto& right) {
            return left.name < right.name;
        });
    } else if (asked.sort == "family") {
        std::ranges::sort(found, [](const auto& left, const auto& right) {
            return left.family_name < right.family_name;
        });
    }
    if (asked.descending)
        std::ranges::reverse(found);
    return found;
}

std::string rows_json(const std::vector<std::string>& written) {
    return rr::array_of(written);
}

/// The service's kind, as the write states it. The retention destination is the
/// row just read, because no lookup of tiers states one.
reporting_messaging::cut_kind_write kind_write_of(const ores::reporting::domain::cut_kind& kind,
                                                   const kind_change& change) {
    reporting_messaging::cut_kind_write write;
    write.code = kind.code;
    write.name = change.name;
    write.description = change.description;
    write.family_code = change.family_code;
    write.approval_kind_code = change.approval_kind_code;
    write.output_is_indicative = change.output_is_indicative;
    write.resolution_rule_code = change.resolution_rule_code;
    write.retention_months = change.retention_months;
    write.retention_destination_code = kind.retention_destination_code;
    write.legal_hold_default = change.legal_hold_default;
    write.display_order = change.display_order;
    return write;
}

/// The checks the form's schema makes, refused by its own sentence.
void check_kind_write(const kind_change& change) {
    if (change.name.size() > 200 || change.description.size() > 2000)
        rr::invalid("A kind of cut states a name of at most 200 characters and a description "
                    "of at most 2000.");
    if (rr::trimmed(change.family_code).empty() || change.family_code.size() > 100)
        rr::invalid("A kind of cut states a family and a resolution rule.");
    if (rr::trimmed(change.resolution_rule_code).empty() ||
        change.resolution_rule_code.size() > 100)
        rr::invalid("A kind of cut states a family and a resolution rule.");
    if (change.approval_kind_code) {
        if (rr::trimmed(*change.approval_kind_code).empty() ||
            change.approval_kind_code->size() > 200)
            rr::invalid("The approval kind a cut is signed by is a code of at most 200 "
                        "characters.");
    }
    if (change.retention_months < 0 || change.retention_months > 1200)
        rr::invalid("A kind of cut is kept for 0 to 1200 months.");
    if (change.display_order < 0 || change.display_order > 1'000'000)
        rr::invalid("A kind of cut is drawn at an order of 0 to 1000000.");
}

}

}

void cut_kind_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/cut-kinds")
            .summary("The kinds of cut a report may select")
            .description("One page of them with the words each of their three codes stands for, "
                         "and the cuts the party holds of each kind.")
            .query_param("offset", "integer", "int32", false, "How many kinds to skip")
            .query_param("limit", "integer", "int32", false, "How many kinds to return")
            .query_param("search", "string", "", false, "A text the code, name or family holds")
            .query_param("sort", "string", "", false, "code, name or family")
            .query_param("descending", "boolean", "", false, "Whether the order is reversed")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_cut_kinds_request>([&] {
                    const auto asked = kinds::kind_page_of(req);
                    const auto rows = kinds::all_kinds(req, session);
                    const auto found = kinds::kinds_matched(rows, asked);

                    const auto first = std::min<std::size_t>(asked.offset, found.size());
                    const auto last = std::min<std::size_t>(first + asked.limit, found.size());
                    std::vector<std::string> written;
                    for (std::size_t i = first; i < last; ++i)
                        written.push_back(cuts::kind_json(found[i]));

                    const auto counts = kinds::kind_cut_counts_of(req, session);
                    return http_response::json("{\"rows\":" + kinds::rows_json(written) +
                                               ",\"total\":" + std::to_string(found.size()) +
                                               ",\"partial\":" +
                                               (counts.partial ? "true" : "false") + "}");
                });
            }));

    add(router->get("/api/reporting/cut-kind-choices")
            .summary("The lookups a kind's form offers")
            .description("The families, the resolution rules, the approval kinds and the "
                         "business centres, and the retention destinations the kinds "
                         "themselves hold, so the form offers what a save will accept.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_cut_kinds_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto lookups = cuts::read_kind_lookups(req, session);

                    std::vector<std::pair<std::string, std::string>> families{lookups.families
                                                                                 .begin(),
                                                                             lookups.families
                                                                                 .end()};
                    std::vector<std::pair<std::string, std::string>> rules{lookups.rules.begin(),
                                                                          lookups.rules.end()};

                    reporting_messaging::list_cut_kinds_request msg;
                    msg.limit = kinds_limit;
                    const auto kinds = forwarding::forward(req, session, msg);

                    // No lookup states the tiers a cut may be kept for, so the
                    // answer is the destinations the kinds themselves hold and
                    // it invents none.
                    std::set<std::string> destinations;
                    for (const auto& kind : kinds.kinds)
                        if (!kind.retention_destination_code.empty())
                            destinations.insert(kind.retention_destination_code);

                    refdata_messaging::list_business_centres_request centres;
                    centres.limit = kinds_limit;
                    const auto answered = forwarding::forward(req, session, centres);

                    const auto codes = [](const auto& pairs) {
                        std::vector<std::string> written;
                        for (const auto& [code, name] : pairs)
                            written.push_back("{\"code\":" + json::write(code) +
                                              ",\"name\":" + json::write(name) + "}");
                        return rr::array_of(written);
                    };

                    std::vector<std::pair<std::string, std::string>> approvals;
                    for (const auto& [code, kind] : lookups.approvals)
                        approvals.emplace_back(code, kind.name);

                    std::vector<std::string> tiers;
                    for (const auto& destination : destinations)
                        tiers.push_back(json::write(destination));

                    std::vector<std::pair<std::string, std::string>> centre_pairs;
                    for (const auto& centre : answered.centres)
                        centre_pairs.emplace_back(centre.code, centre.code);

                    return http_response::json(
                        "{\"families\":" + codes(families) +
                        ",\"resolutionRules\":" + codes(rules) +
                        ",\"approvalKinds\":" + codes(approvals) +
                        ",\"retentionDestinations\":" + rr::array_of(tiers) +
                        ",\"businessCentres\":" + codes(centre_pairs) + "}");
                });
            }));

    add(router->get("/api/reporting/cut-kinds/{code}")
            .summary("One kind of cut and the cuts the party holds of it")
            .description("The kind with the words its codes stand for, and its cuts newest "
                         "first, which a page groups by the period above its frequency.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_cut_kinds_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto code = req.get_path_param("code");
                    if (code.empty() || code.size() > 100)
                        rr::invalid("The address names no code of a kind of cut.");

                    const auto kind = cuts::read_one_kind(req, session, code);
                    if (!kind)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "There is no such cut type, or you may not see it.");

                    reporting_messaging::list_cut_rows_request ask;
                    ask.kind_code = code;
                    ask.limit = cuts_of_a_kind_page;
                    const auto rows = forwarding::forward(req, session, ask);

                    std::vector<std::string> written;
                    for (const auto& row : rows.rows)
                        written.push_back(cuts::cut_row_json(row));
                    const auto partial =
                        rows.total > static_cast<int>(rows.rows.size()) ? "true" : "false";

                    return http_response::json("{\"kind\":" + cuts::kind_json(*kind) +
                                               ",\"cuts\":" + rr::array_of(written) +
                                               ",\"partial\":" + partial + "}");
                });
            }));

    add(router->put("/api/reporting/cut-kinds/{code}")
            .summary("Write the naming and the rules of one kind of cut")
            .description("The write names the version the form was filled from, so a kind "
                         "somebody else changed in between is a conflict rather than a silent "
                         "overwrite.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::put_cut_kind_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto code = req.get_path_param("code");
                    if (code.empty() || code.size() > 100)
                        rr::invalid("The address names no code of a kind of cut.");

                    const auto body = rr::body_of<kinds::kind_edit_body>(req,
                                                             {"version", "change", "intent"},
                                                             "A kind of cut states the version it "
                                                             "was read at, its fields and why it "
                                                             "changed.");
                    kinds::check_kind_write(body.change);
                    const auto intent = rr::intent_of(body.intent);
                    if (!intent)
                        rr::invalid("A kind of cut states why it changed, in 1 to 200 "
                                    "characters.");

                    const auto read = kinds::domain_kind_of(req, session, code);
                    if (!read)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "There is no such cut type, or you may not see it.");

                    reporting_messaging::put_cut_kind_request msg;
                    msg.change.write = kinds::kind_write_of(*read, body.change);
                    msg.change.precondition.kind =
                        ores::utility::domain::precondition_kind::must_match_version;
                    msg.change.precondition.version = body.version;
                    msg.intent = *intent;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    const auto written = cuts::read_one_kind(req, session, code);
                    if (!written)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "There is no such cut type, or you may not see it.");
                    return http_response::json(cuts::kind_json(*written));
                });
            }));

    add(router->delete_("/api/reporting/cut-kinds/{code}")
            .summary("Remove one kind of cut no cut was taken under")
            .description("A kind a cut names cannot be removed: the cut keeps the facts it "
                         "copied and its label reads the kind's family, so the store refuses "
                         "the removal and the screen states its sentence.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::delete_cut_kind_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto code = req.get_path_param("code");
                    if (code.empty() || code.size() > 100)
                        rr::invalid("The address names no code of a kind of cut.");

                    const auto body = rr::body_of<kinds::kind_removal_body>(req,
                                                                 {"version", "intent"},
                                                                 "A removal states the version it "
                                                                 "was read at and why.");
                    const auto intent = rr::intent_of(body.intent);
                    if (!intent)
                        rr::invalid("A removal states why it is made, in 1 to 200 characters.");

                    reporting_messaging::delete_cut_kind_request msg;
                    msg.removal.key.code = code;
                    msg.removal.precondition.kind =
                        ores::utility::domain::precondition_kind::must_match_version;
                    msg.removal.precondition.version = body.version;
                    msg.intent = *intent;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return rr::no_content();
                });
            }));
}

}
