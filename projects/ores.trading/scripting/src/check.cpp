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
#include "ores.trading.scripting/check.hpp"
#include "spirit_columns.hpp"
#include <algorithm>
#include <charconv>
#include <cmath>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace ores::trading::scripting {

namespace {

/// The place of a node's text, in code points.
std::optional<position> start_of(const ASTNode& node, std::string_view text) {
    const auto& l = node.locationInfo;
    if (!l.initialised)
        return std::nullopt;
    const auto line = spirit_columns::line_of(text, l.lineStart).value_or("");
    return position{.line = l.lineStart,
                    .column = spirit_columns::code_point_column(line, l.columnStart)};
}

std::optional<position> end_of(const ASTNode& node, std::string_view text) {
    const auto& l = node.locationInfo;
    if (!l.initialised)
        return std::nullopt;
    const auto line = spirit_columns::line_of(text, l.lineEnd).value_or("");
    return position{.line = l.lineEnd,
                    .column = spirit_columns::code_point_column(line, l.columnEnd)};
}

std::string described(const std::string& type, bool is_array) {
    return is_array ? "a list of " + type : "a " + type;
}

/// The names a script declares, each with its declaration, and the names it uses.
struct name_uses final {
    std::map<std::string, const ASTNode*> declared;
    std::set<std::string> used;

    void collect(const ASTNodePtr& node) {
        if (!node)
            return;
        if (dynamic_cast<const DeclarationNumberNode*>(node.get())) {
            for (const auto& arg : node->args) {
                const auto* v = dynamic_cast<const VariableNode*>(arg.get());
                if (!v)
                    continue;
                declared.emplace(v->name, node.get());
                // A declared list's size is a use of what it is sized by.
                for (const auto& index : v->args)
                    collect(index);
            }
            return;
        }
        if (const auto* v = dynamic_cast<const VariableNode*>(node.get()))
            used.insert(v->name);
        else if (const auto* s = dynamic_cast<const SizeOpNode*>(node.get()))
            used.insert(s->name);
        else if (const auto* d = dynamic_cast<const FunctionDateIndexNode*>(node.get()))
            used.insert(d->name);
        else if (const auto* l = dynamic_cast<const LoopNode*>(node.get()))
            used.insert(l->name);
        for (const auto& arg : node->args)
            collect(arg);
    }
};

/*
 * Evaluates a REQUIRE statement over the trade's values, where it can: every
 * name the statement reads must be a number the trade states, or the size of a
 * list it states. Anything else, a date, an index, a variable the script
 * computes, leaves the statement to ORE.
 */
class rule_evaluation final {
public:
    rule_evaluation(const std::map<std::string, const script_variable*>& variables,
                    const std::set<std::string>& own)
        : variables_(variables)
        , own_(own) {}

    std::optional<bool> condition(const ASTNode* n) const {
        if (!n)
            return std::nullopt;
        const auto& a = n->args;
        const auto compare = [&](auto op) -> std::optional<bool> {
            const auto l = term(a[0].get());
            const auto r = term(a[1].get());
            if (!l || !r)
                return std::nullopt;
            return op(*l, *r);
        };
        if (dynamic_cast<const ConditionEqNode*>(n))
            return compare([](double l, double r) { return l == r; });
        if (dynamic_cast<const ConditionNeqNode*>(n))
            return compare([](double l, double r) { return l != r; });
        if (dynamic_cast<const ConditionLtNode*>(n))
            return compare([](double l, double r) { return l < r; });
        if (dynamic_cast<const ConditionLeqNode*>(n))
            return compare([](double l, double r) { return l <= r; });
        if (dynamic_cast<const ConditionGtNode*>(n))
            return compare([](double l, double r) { return l > r; });
        if (dynamic_cast<const ConditionGeqNode*>(n))
            return compare([](double l, double r) { return l >= r; });
        if (dynamic_cast<const ConditionNotNode*>(n)) {
            const auto c = condition(a[0].get());
            return c ? std::optional<bool>(!*c) : std::nullopt;
        }
        if (dynamic_cast<const ConditionAndNode*>(n) || dynamic_cast<const ConditionOrNode*>(n)) {
            const bool both = dynamic_cast<const ConditionAndNode*>(n) != nullptr;
            const auto l = condition(a[0].get());
            const auto r = condition(a[1].get());
            // One known side can settle the statement on its own.
            if (both && ((l && !*l) || (r && !*r)))
                return false;
            if (!both && ((l && *l) || (r && *r)))
                return true;
            if (!l || !r)
                return std::nullopt;
            return both ? (*l && *r) : (*l || *r);
        }
        return std::nullopt;
    }

private:
    std::optional<double> term(const ASTNode* n) const {
        if (!n)
            return std::nullopt;
        const auto& a = n->args;
        if (const auto* c = dynamic_cast<const ConstantNumberNode*>(n))
            return c->value;
        if (const auto* v = dynamic_cast<const VariableNode*>(n)) {
            const auto* var = known(v->name);
            if (!var || var->is_array || var->type != "Number" || var->values.size() != 1 ||
                (!a.empty() && a[0]))
                return std::nullopt;
            return number(var->values.front());
        }
        if (const auto* s = dynamic_cast<const SizeOpNode*>(n)) {
            const auto* var = known(s->name);
            if (!var || !var->is_array)
                return std::nullopt;
            return static_cast<double>(var->values.size());
        }
        const auto arithmetic = [&](auto op) -> std::optional<double> {
            const auto l = term(a[0].get());
            const auto r = term(a[1].get());
            if (!l || !r)
                return std::nullopt;
            return op(*l, *r);
        };
        if (dynamic_cast<const OperatorPlusNode*>(n))
            return arithmetic([](double l, double r) { return l + r; });
        if (dynamic_cast<const OperatorMinusNode*>(n))
            return arithmetic([](double l, double r) { return l - r; });
        if (dynamic_cast<const OperatorMultiplyNode*>(n))
            return arithmetic([](double l, double r) { return l * r; });
        if (dynamic_cast<const OperatorDivideNode*>(n))
            return arithmetic([](double l, double r) { return l / r; });
        if (dynamic_cast<const FunctionMaxNode*>(n))
            return arithmetic([](double l, double r) { return std::max(l, r); });
        if (dynamic_cast<const FunctionMinNode*>(n))
            return arithmetic([](double l, double r) { return std::min(l, r); });
        if (dynamic_cast<const NegateNode*>(n)) {
            const auto v = term(a[0].get());
            return v ? std::optional<double>(-*v) : std::nullopt;
        }
        if (dynamic_cast<const FunctionAbsNode*>(n)) {
            const auto v = term(a[0].get());
            return v ? std::optional<double>(std::abs(*v)) : std::nullopt;
        }
        return std::nullopt;
    }

    const script_variable* known(const std::string& name) const {
        if (own_.contains(name))
            return nullptr;
        const auto it = variables_.find(name);
        return it == variables_.end() ? nullptr : it->second;
    }

    static std::optional<double> number(const std::string& text) {
        double out = 0;
        const auto* first = text.data();
        const auto* last = first + text.size();
        const auto result = std::from_chars(first, last, out);
        if (result.ec != std::errc{} || result.ptr != last)
            return std::nullopt;
        return out;
    }

    const std::map<std::string, const script_variable*>& variables_;
    const std::set<std::string>& own_;
};

void find_requires(const ASTNodePtr& node, std::vector<const ASTNode*>& out) {
    if (!node)
        return;
    if (dynamic_cast<const RequireNode*>(node.get()))
        out.push_back(node.get());
    for (const auto& arg : node->args)
        find_requires(arg, out);
}

diagnostic variable_finding(std::string code, std::string subject, std::string message) {
    diagnostic d;
    d.code = std::move(code);
    d.severity = code_of(d.code)->severity;
    d.subject = std::move(subject);
    d.message = std::move(message);
    return d;
}

diagnostic
located_finding(std::string code, const ASTNode& node, std::string_view text, std::string message) {
    diagnostic d;
    d.code = std::move(code);
    d.severity = code_of(d.code)->severity;
    d.start = start_of(node, text);
    d.end = end_of(node, text);
    d.message = std::move(message);
    return d;
}

}

std::vector<diagnostic> check(const ASTNodePtr& tree,
                              std::string_view text,
                              std::span<const script_input> inputs,
                              std::span<const script_variable> variables) {
    std::vector<diagnostic> out;
    std::map<std::string, const script_variable*> by_name;
    for (const auto& v : variables)
        by_name.emplace(v.name, &v);
    std::set<std::string> input_names;

    for (const auto& in : inputs) {
        input_names.insert(in.name);
        const auto it = by_name.find(in.name);
        if (it == by_name.end()) {
            out.push_back(variable_finding("SCR2001",
                                           in.name,
                                           "the trade states no value for this input, " +
                                               described(in.type, in.is_array)));
            continue;
        }
        const auto& v = *it->second;
        if (v.type != in.type)
            out.push_back(variable_finding("SCR2003",
                                           in.name,
                                           "the trade states " + v.type +
                                               ", but the script reads " + in.type));
        if (v.is_array != in.is_array)
            out.push_back(variable_finding(
                "SCR2004",
                in.name,
                in.is_array ? "the script reads a list, but the trade states one value" :
                              "the script reads one value, but the trade states a list"));
    }
    for (const auto& v : variables)
        if (!input_names.contains(v.name))
            out.push_back(
                variable_finding("SCR2002", v.name, "the script never reads this variable"));

    name_uses uses;
    uses.collect(tree);
    std::set<std::string> own;
    for (const auto& [name, declaration] : uses.declared)
        own.insert(name);

    std::vector<const ASTNode*> require_nodes;
    find_requires(tree, require_nodes);
    const rule_evaluation evaluation(by_name, own);
    for (const auto* r : require_nodes)
        if (!r->args.empty() && evaluation.condition(r->args[0].get()) == false)
            out.push_back(
                located_finding("SCR3001", *r, text, "the trade's values break this REQUIRE"));

    for (const auto& [name, declaration] : uses.declared)
        if (!uses.used.contains(name))
            out.push_back(located_finding(
                "SCR4001", *declaration, text, name + " is declared and never used"));

    // Located findings first, by line and column; then findings about a
    // variable, by its name.
    std::ranges::stable_sort(out, [](const diagnostic& l, const diagnostic& r) {
        if (l.start.has_value() != r.start.has_value())
            return l.start.has_value();
        if (l.start)
            return std::pair(l.start->line, l.start->column.value_or(0)) <
                   std::pair(r.start->line, r.start->column.value_or(0));
        return l.subject.value_or("") < r.subject.value_or("");
    });
    return out;
}

}
