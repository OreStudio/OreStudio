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
#include "ores.http.api/mcp/catalogue_tools.hpp"
#include "ores.http.api/domain/catalogue_rules.hpp"
#include "ores.security/authorization/agent_withholding.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <algorithm>
#include <array>
#include <chrono>
#include <map>
#include <utility>

namespace ores::http::mcp {

using namespace ores::logging;
using domain::http_request;
using domain::http_response;
using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

namespace {

constexpr auto purposes = std::to_array<std::pair<std::string_view, std::string_view>>({
    {"access", "accounts, roles, permissions and who may do what"},
    {"agile", "the project's own sprints, stories and tasks"},
    {"analytics", "sensitivities, risk measures and their templates"},
    {"assets", "images and other files the system shows"},
    {"auth", "signing in, sessions and agent grants"},
    {"changes", "change reasons and the stream of changes"},
    {"grid", "compute jobs, workers and application versions"},
    {"market-data", "quotes, curves, surfaces, fixings and their providers"},
    {"operations", "the server's health, storage and operational records"},
    {"qa", "data quality checks and their results"},
    {"reference-data", "currencies, countries, calendars, books, parties and conventions"},
    {"reporting", "reports, schedules, runs and pricing engines"},
    {"simulation", "synthetic feeds, processes and their calibration"},
    {"tenancy", "tenants and their provisioning"},
    {"trading", "trades, portfolios, structures and their authorisation"},
    {"workflow", "requests, approvals, notifications and retries"},
});

std::string_view purpose_of(std::string_view area) {
    for (const auto& [name, purpose] : purposes)
        if (name == area)
            return purpose;
    return "";
}

tool_result refused(std::string text) {
    return tool_result{.text = std::move(text), .is_error = true};
}

std::string encoded(std::string_view text) {
    static constexpr std::string_view digits = "0123456789ABCDEF";
    std::string out;
    for (const auto c : text) {
        const auto u = static_cast<unsigned char>(c);
        if (std::isalnum(u) || c == '-' || c == '_' || c == '.' || c == '~') {
            out.push_back(c);
        } else {
            out.push_back('%');
            out.push_back(digits[u >> 4]);
            out.push_back(digits[u & 0x0f]);
        }
    }
    return out;
}

/// An argument as the text a path or a query carries.
std::optional<std::string> text_of(const json_value& value) {
    if (value.is_string())
        return std::string(*value.text());
    if (value.is_integer())
        return std::to_string(*value.integer());
    if (value.is_bool())
        return *value.flag() ? "true" : "false";
    if (value.is_number())
        return value.write();
    return std::nullopt;
}

std::size_t page_of(const json_value& arguments) {
    const auto asked = arguments.member("page").integer().value_or(1);
    return asked < 1 ? 1 : static_cast<std::size_t>(asked);
}

/// One page of lines, with a header that says where it is.
std::string paged(std::string_view title,
                  const std::vector<std::string>& lines,
                  std::size_t page,
                  std::size_t size) {
    const auto pages = std::max<std::size_t>(1, (lines.size() + size - 1) / size);
    page = std::min(page, pages);
    std::string out(title);
    if (pages > 1)
        out += " (page " + std::to_string(page) + " of " + std::to_string(pages) +
               "; ask for another with page)";
    out += ":\n";
    for (std::size_t i = (page - 1) * size; i < lines.size() && i < page * size; ++i)
        out += lines[i] + "\n";
    return out;
}

std::string line_of(const operation& op) {
    return op.tool + (op.writes ? " [writes]" : " [reads]") + ": " +
           (op.summary.empty() ? std::string(domain::method_name(op.method)) + " " + op.pattern :
                                 op.summary);
}

std::string schema_answer(const operation& op, std::string_view why) {
    return std::string(why) + "\nThe arguments of " + op.tool + " are " + input_schema_of(op) +
           "\nFor example: " + example_of(op);
}

}

catalogue_tools::catalogue_tools(std::shared_ptr<const operation_catalogue> catalogue,
                                 catalogue_tools_options options)
    : catalogue_(std::move(catalogue))
    , options_(std::move(options)) {}

bool catalogue_tools::may_write(const http_request& caller) {
    if (!caller.authenticated_user)
        return false;
    return std::ranges::any_of(caller.authenticated_user->roles, [](const std::string& code) {
        return !ores::security::authorization::is_read_permission(code);
    });
}

std::vector<tool> catalogue_tools::list(const http_request&) const {
    const auto paging =
        R"("page":{"type":"integer","minimum":1,"description":"The page of a long list, from 1."})";
    return {
        tool{.name = "list_areas",
             .title = "List areas",
             .description = "Lists the areas of ORE's operations. Start here, then list an area's "
                            "entities.",
             .input_schema = R"({"type":"object","properties":{},"additionalProperties":false})"},
        tool{.name = "list_entities",
             .title = "List an area's entities",
             .description = "Lists the entities of one area, each with how many operations it has.",
             .input_schema =
                 std::string(R"({"type":"object","properties":{"area":{"type":"string"},)") +
                 paging + R"(},"required":["area"],"additionalProperties":false})"},
        tool{
            .name = "list_operations",
            .title = "List an entity's operations",
            .description = "Lists the operations on one entity of an area, each with its name, "
                           "whether it reads or writes, and what it does.",
            .input_schema =
                std::string(
                    R"({"type":"object","properties":{"area":{"type":"string"},"entity":{"type":"string"},)") +
                paging + R"(},"required":["area","entity"],"additionalProperties":false})"},
        tool{
            .name = "find_operations",
            .title = "Find operations by words",
            .description = "Finds the five operations that best match a few words, such as "
                           "\"list currencies\" or \"book a trade\".",
            .input_schema =
                R"({"type":"object","properties":{"words":{"type":"string"}},"required":["words"],"additionalProperties":false})"},
        tool{
            .name = "call_operation",
            .title = "Call an operation",
            .description = "Calls one operation by its name with its arguments. Call it with no "
                           "arguments to learn what it takes. A write waits for your person's "
                           "approval and answers the request it raised.",
            .input_schema =
                R"({"type":"object","properties":{"name":{"type":"string"},"arguments":{"type":"object"}},"required":["name"],"additionalProperties":false})",
            .read_only = false},
    };
}

boost::asio::awaitable<std::optional<tool_result>> catalogue_tools::call(
    const http_request& caller, std::string_view name, json_value arguments) const {
    if (name == "list_areas")
        co_return areas(caller);
    if (name == "list_entities")
        co_return entities(caller, arguments);
    if (name == "list_operations")
        co_return operations(caller, arguments);
    if (name == "find_operations")
        co_return found(caller, arguments);
    if (name == "call_operation")
        co_return co_await called(caller, arguments);
    co_return std::nullopt;
}

tool_result catalogue_tools::areas(const http_request& caller) const {
    const auto writes = may_write(caller);
    std::map<std::string, std::size_t> counts;
    for (const auto& op : catalogue_->operations())
        if (writes || !op.writes)
            ++counts[op.area];
    std::string out = "Areas, each with what it holds and how many operations you may call:\n";
    for (const auto area : domain::published_areas())
        if (const auto count = counts.find(std::string(area)); count != counts.end())
            out += std::string(area) + ": " + std::string(purpose_of(area)) + " (" +
                   std::to_string(count->second) + ")\n";
    return tool_result{.text = out};
}

tool_result catalogue_tools::entities(const http_request& caller,
                                      const json_value& arguments) const {
    const auto area = arguments.member("area").text_or();
    const auto writes = may_write(caller);
    std::map<std::string, std::size_t> counts;
    for (const auto& op : catalogue_->operations())
        if (op.area == area && (writes || !op.writes))
            ++counts[op.entity];
    if (counts.empty())
        return refused("No area named \"" + area +
                       "\" has operations you may call; list_areas names them.");
    std::vector<std::string> lines;
    for (const auto& [entity, count] : counts)
        lines.push_back(entity + " (" + std::to_string(count) + ")");
    return tool_result{.text = paged("Entities of " + area + ", each with its operations",
                                     lines,
                                     page_of(arguments),
                                     options_.page_size)};
}

tool_result catalogue_tools::operations(const http_request& caller,
                                        const json_value& arguments) const {
    const auto area = arguments.member("area").text_or();
    const auto entity = arguments.member("entity").text_or();
    const auto writes = may_write(caller);
    std::vector<std::string> lines;
    for (const auto& op : catalogue_->operations())
        if (op.area == area && op.entity == entity && (writes || !op.writes))
            lines.push_back(line_of(op));
    if (lines.empty())
        return refused("No entity named \"" + entity + "\" in \"" + area +
                       "\" has operations you may call; list_entities names them.");
    std::ranges::sort(lines);
    return tool_result{
        .text = paged("Operations on " + entity, lines, page_of(arguments), options_.page_size)};
}

tool_result catalogue_tools::found(const http_request& caller, const json_value& arguments) const {
    const auto words = arguments.member("words").text_or();
    if (search_terms(words).empty())
        return refused("find_operations needs a few words, such as \"list currencies\".");
    const auto writes = may_write(caller);
    const auto best =
        catalogue_->search(words, 5, [&](const operation& op) { return writes || !op.writes; });
    if (best.empty())
        return tool_result{.text = "No operation matches those words; try others, or list_areas."};
    std::string out = "The best matches, best first:\n";
    for (const auto* op : best)
        out += line_of(*op) + " (" + op->area + " / " + op->entity + ")\n";
    return tool_result{.text = out};
}

boost::asio::awaitable<tool_result> catalogue_tools::called(const http_request& caller,
                                                            json_value arguments) const {
    if (!caller.authenticated_user)
        co_return refused("call_operation is served to an agent's token.");
    const auto name = arguments.member("name").text_or();
    const auto* op = catalogue_->find(name);
    if (op == nullptr)
        co_return refused("No operation you may call is named \"" + name +
                          "\"; find_operations finds one.");

    // Each call that names an operation is recorded as the agent's direct
    // call would be, a refusal or a hold included, so its person sees it.
    const auto started = std::chrono::steady_clock::now();
    const auto note = [&](int status) {
        if (!options_.record)
            return;
        options_.record(domain::exchanged_call{
            .claims = *caller.authenticated_user,
            .method = std::string(domain::method_name(op->method)),
            .route = op->pattern,
            .status = status,
            .duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now() - started)});
    };

    if (op->writes && !may_write(caller)) {
        note(403);
        co_return refused("No operation you may call is named \"" + name +
                          "\"; find_operations finds one.");
    }
    if (!op->required_roles.empty() &&
        std::ranges::none_of(op->required_roles, [&](const std::string& role) {
            return std::ranges::find(caller.authenticated_user->roles, role) !=
                   caller.authenticated_user->roles.end();
        })) {
        note(403);
        co_return refused("Your person's roles do not reach " + op->tool + ".");
    }

    const auto given = arguments.member("arguments");
    if (!given.exists() || !given.is_object() || given.size() == 0) {
        if (!op->path_params.empty() || (op->body && op->body->required) ||
            std::ranges::any_of(op->query_params, [](const auto& q) { return q.required; }))
            co_return tool_result{.text = schema_answer(*op, op->tool + " needs arguments.")};
    }
    const auto misread = [&](const std::string& argument) {
        return tool_result{.text = schema_answer(*op,
                                                 "The argument " + argument +
                                                     " is missing, or is not text, a number "
                                                     "or a truth value."),
                           .is_error = true};
    };

    http_request inner;
    inner.method = op->method;
    inner.authenticated_user = caller.authenticated_user;
    inner.remote_address = caller.remote_address;
    if (const auto token = caller.get_header("authorization"); !token.empty())
        inner.headers["authorization"] = token;

    std::string path = op->pattern;
    for (const auto& param : op->path_params) {
        const auto value = text_of(given.member(param));
        if (!value)
            co_return misread(param);
        inner.path_params[param] = *value;
        const auto at = path.find("{" + param + "}");
        if (at != std::string::npos)
            path.replace(at, param.size() + 2, encoded(*value));
    }
    std::string query;
    for (const auto& param : op->query_params) {
        const auto argument = given.member(param.name);
        if (!argument.exists()) {
            if (param.required)
                co_return misread(param.name);
            continue;
        }
        const auto value = text_of(argument);
        if (!value)
            co_return misread(param.name);
        inner.query_params[param.name] = *value;
        query += (query.empty() ? "?" : "&") + encoded(param.name) + "=" + encoded(*value);
    }
    if (op->body) {
        const auto body = given.member("body");
        if (body.exists()) {
            inner.body = body.write();
            inner.headers["content-type"] = "application/json";
        } else if (op->body->required) {
            co_return misread("body");
        }
    }
    inner.target = path + query;

    if (op->writes && options_.hold) {
        if (auto held = options_.hold(inner, op->pattern)) {
            ORES_LOG_SEV(lg(), info) << "Held an agent's call of " << op->tool;
            note(static_cast<int>(held->status));
            co_return tool_result{.text =
                                      "The write waits for your person's approval: " + held->body,
                                  .is_error = static_cast<int>(held->status) >= 400};
        }
    }

    auto answer = co_await op->handler(inner);
    const auto status = static_cast<int>(answer.status);
    note(status);

    std::string text = "HTTP " + std::to_string(status) + "\n";
    if (answer.body.size() > options_.answer_limit) {
        auto cut = options_.answer_limit;
        while (cut > 0 && (static_cast<unsigned char>(answer.body[cut]) & 0xC0) == 0x80)
            --cut;
        text += answer.body.substr(0, cut);
        text += "\n[cut: " + std::to_string(answer.body.size() - cut) +
                " more characters; ask for less, such as a smaller page]";
    } else {
        text += answer.body;
    }
    if (status == 400)
        text += "\n" + schema_answer(*op, "The operation refused its arguments.");
    co_return tool_result{.text = std::move(text), .is_error = status >= 400};
}

}
