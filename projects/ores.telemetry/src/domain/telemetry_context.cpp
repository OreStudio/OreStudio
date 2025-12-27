/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/domain/telemetry_context.hpp"
#include "ores.telemetry/domain/semantic_conventions.hpp"
#include "ores.telemetry/generators/trace_id_generator.hpp"
#include "ores.telemetry/generators/span_id_generator.hpp"

namespace ores::telemetry::domain {

namespace {

// File-scoped generators to ensure single instance and avoid ID collisions
generators::trace_id_generator g_trace_gen;
generators::span_id_generator g_span_gen;

}

telemetry_context::telemetry_context(span_context ctx,
                                     std::shared_ptr<resource> res)
    : ctx_(std::move(ctx)), resource_(std::move(res)) {}

telemetry_context telemetry_context::create_root(std::shared_ptr<resource> res) {
    span_context ctx;
    ctx.trace = g_trace_gen();
    ctx.span = g_span_gen();
    ctx.trace_flags = 0x01; // sampled

    return telemetry_context(ctx, std::move(res));
}

const span_context& telemetry_context::context() const {
    return ctx_;
}

const trace_id& telemetry_context::get_trace_id() const {
    return ctx_.trace;
}

const span_id& telemetry_context::get_span_id() const {
    return ctx_.span;
}

const resource& telemetry_context::get_resource() const {
    return *resource_;
}

std::shared_ptr<resource> telemetry_context::resource_ptr() const {
    return resource_;
}

std::pair<telemetry_context, span> telemetry_context::start_span(
    std::string_view name,
    span_kind kind) const {

    const auto new_span_id = g_span_gen();

    // Create the new context with same trace but new span
    span_context new_ctx;
    new_ctx.trace = ctx_.trace;
    new_ctx.span = new_span_id;
    new_ctx.trace_flags = ctx_.trace_flags;

    // Create the span data structure
    span new_span;
    new_span.context = new_ctx;
    new_span.parent_span_id = ctx_.span; // Current span becomes parent
    new_span.name = std::string(name);
    new_span.kind = kind;
    new_span.start_time = std::chrono::system_clock::now();

    return {telemetry_context(new_ctx, resource_), std::move(new_span)};
}

std::pair<telemetry_context, span> telemetry_context::start_linked_trace(
    std::string_view name,
    span_kind kind) const {

    const auto new_trace_id = g_trace_gen();
    const auto new_span_id = g_span_gen();

    // Create a completely new context
    span_context new_ctx;
    new_ctx.trace = new_trace_id;
    new_ctx.span = new_span_id;
    new_ctx.trace_flags = ctx_.trace_flags;

    // Create the span with a link back to the originating span
    span new_span;
    new_span.context = new_ctx;
    // No parent - this is a root span of a new trace
    new_span.name = std::string(name);
    new_span.kind = kind;
    new_span.start_time = std::chrono::system_clock::now();

    // Add a link to the originating span
    span_link link;
    link.context = ctx_;
    link.attrs[std::string(semconv::link::relationship)] =
        std::string(semconv::link::triggered_by);
    new_span.links.push_back(std::move(link));

    return {telemetry_context(new_ctx, resource_), std::move(new_span)};
}

bool telemetry_context::is_valid() const {
    return ctx_.is_valid() && resource_ != nullptr;
}

}
