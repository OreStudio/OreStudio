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
#include "ores.trading.scripting/inputs.hpp"
#include "spirit_columns.hpp"
#include <algorithm>
#include <cctype>
#include <initializer_list>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace ores::trading::scripting {

namespace {

/// The text a node spans, with its whitespace collapsed to single spaces.
std::string text_of(const ASTNode& node, std::string_view text) {
    const auto& l = node.locationInfo;
    if (!l.initialised)
        return {};
    const auto begin = spirit_columns::byte_offset(text, l.lineStart, l.columnStart);
    const auto end = std::max(begin, spirit_columns::byte_offset(text, l.lineEnd, l.columnEnd));
    std::string out;
    bool space = false;
    for (const char c : text.substr(begin, end - begin)) {
        if (std::isspace(static_cast<unsigned char>(c))) {
            space = !out.empty();
            continue;
        }
        if (space)
            out += ' ';
        out += c;
        space = false;
    }
    return out;
}

/// The first name a rule's text states, which is the input the rule is about.
std::string first_name(std::string_view rule) {
    std::size_t at = 0;
    while (at < rule.size() &&
           !(std::isalpha(static_cast<unsigned char>(rule[at])) || rule[at] == '_'))
        ++at;
    std::size_t end = at;
    while (end < rule.size() &&
           (std::isalnum(static_cast<unsigned char>(rule[end])) || rule[end] == '_'))
        ++end;
    return std::string(rule.substr(at, end - at));
}

/// The variable a call argument is, when it is one variable, possibly indexed.
const VariableNode* bare(const ASTNodePtr& arg) {
    return dynamic_cast<const VariableNode*>(arg.get());
}

/*
 * The names a variant declares and assigns, which are its own and not the
 * trade's, are collected over the whole variant before any input is noted, so a
 * name used before its declaration is still the variant's.
 */
struct own_names final {
    std::set<std::string> names;

    void collect(const ASTNodePtr& node) {
        if (!node)
            return;
        if (dynamic_cast<const DeclarationNumberNode*>(node.get()) ||
            dynamic_cast<const AssignmentNode*>(node.get())) {
            const auto& args = node->args;
            // A declaration names each of its arguments; an assignment its first.
            const auto count =
                dynamic_cast<const AssignmentNode*>(node.get()) ? std::size_t{1} : args.size();
            for (std::size_t i = 0; i < count && i < args.size(); ++i)
                if (const auto* v = bare(args[i]))
                    names.insert(v->name);
        }
        for (const auto& arg : node->args)
            collect(arg);
    }
};

class input_walk final {
public:
    input_walk(const std::set<std::string>& own, std::string_view text)
        : own_(own)
        , text_(text) {}

    void walk(const ASTNodePtr& node) {
        if (!node)
            return;
        const auto* n = node.get();
        const auto& args = node->args;
        if (const auto* v = dynamic_cast<const VariableNode*>(n)) {
            note(v->name);
            // ORE pads a node's arguments with nulls up to its most; an index is
            // a variable's first argument that is there.
            if (!args.empty() && args[0])
                arrays_.insert(v->name);
        } else if (const auto* s = dynamic_cast<const SizeOpNode*>(n)) {
            note(s->name);
            arrays_.insert(s->name);
        } else if (dynamic_cast<const VarEvaluationNode*>(n) && !args.empty()) {
            if (const auto* v = bare(args[0]))
                note(v->name, "Index");
            type_arguments(args, {{1, "Event"}, {2, "Event"}});
        } else if (dynamic_cast<const FunctionPayNode*>(n) ||
                   dynamic_cast<const FunctionLogPayNode*>(n)) {
            type_arguments(args, {{1, "Event"}, {2, "Event"}, {3, "Currency"}});
        } else if (dynamic_cast<const FunctionAboveProbNode*>(n) ||
                   dynamic_cast<const FunctionBelowProbNode*>(n)) {
            type_arguments(args, {{0, "Index"}, {1, "Event"}, {2, "Event"}});
        } else if (dynamic_cast<const FunctionDcfNode*>(n) ||
                   dynamic_cast<const FunctionDaysNode*>(n)) {
            type_arguments(args, {{0, "Daycounter"}, {1, "Event"}, {2, "Event"}});
        } else if (dynamic_cast<const FunctionNpvNode*>(n)) {
            type_arguments(args, {{1, "Event"}});
        } else if (dynamic_cast<const FunctionFwdCompNode*>(n) ||
                   dynamic_cast<const FunctionFwdAvgNode*>(n) ||
                   dynamic_cast<const HistFixingNode*>(n)) {
            type_arguments(args, {{0, "Index"}});
        } else if (const auto* d = dynamic_cast<const FunctionDateIndexNode*>(n)) {
            type_arguments(args, {{0, "Event"}});
            note(d->name, "Event");
            arrays_.insert(d->name);
        } else if (dynamic_cast<const RequireNode*>(n) && !args.empty()) {
            const auto rule = text_of(*args[0], text_);
            // A rule is about the first name its condition states, and the first
            // rule about a name is the one kept, as the library's inputs were
            // mined; REQUIRE a < b is a rule about a alone.
            if (!rule.empty())
                rules_.emplace(first_name(rule), rule);
        }
        for (const auto& arg : args)
            walk(arg);
    }

    std::vector<script_input> inputs() const {
        std::vector<script_input> out;
        for (const auto& name : order_) {
            script_input in{.name = name,
                            .type = types_.at(name).value_or("Number"),
                            .is_array = arrays_.contains(name),
                            .rule = std::nullopt};
            if (const auto r = rules_.find(name); r != rules_.end())
                in.rule = r->second;
            out.push_back(std::move(in));
        }
        return out;
    }

private:
    /*
     * A call types its arguments as it is met, before the arguments that come
     * first in its text are walked, so an input's first use is where the call
     * names it.
     */
    void type_arguments(const std::vector<ASTNodePtr>& args,
                        std::initializer_list<std::pair<std::size_t, const char*>> typed) {
        for (const auto& [position, type] : typed)
            if (position < args.size())
                if (const auto* v = bare(args[position]))
                    note(v->name, type);
    }

    void note(const std::string& name, std::optional<std::string> type = std::nullopt) {
        if (name == "TODAY" || own_.contains(name))
            return;
        auto [it, added] = types_.emplace(name, std::nullopt);
        if (added)
            order_.push_back(name);
        // A use that states a type wins over one that does not, and the first
        // typed use wins over a later one; an input no use types is a Number.
        if (type && !it->second)
            it->second = std::move(type);
    }

    const std::set<std::string>& own_;
    std::string_view text_;
    std::vector<std::string> order_;
    std::map<std::string, std::optional<std::string>> types_;
    std::set<std::string> arrays_;
    std::map<std::string, std::string> rules_;
};

}

std::vector<script_input> inputs_of(const ASTNodePtr& tree, std::string_view text) {
    own_names own;
    own.collect(tree);
    input_walk walk(own.names, text);
    walk.walk(tree);
    return walk.inputs();
}

std::vector<script_input> merge_inputs(std::span<const std::vector<script_input>> variants) {
    std::vector<script_input> out;
    for (const auto& variant : variants)
        for (const auto& in : variant) {
            auto it = std::ranges::find(out, in.name, &script_input::name);
            if (it == out.end()) {
                out.push_back(in);
                continue;
            }
            // A variant that types an input, or indexes it, says more of it than
            // one that only reads it.
            if (it->type == "Number" && in.type != "Number")
                it->type = in.type;
            it->is_array = it->is_array || in.is_array;
            if (!it->rule && in.rule)
                it->rule = in.rule;
        }
    return out;
}

}
