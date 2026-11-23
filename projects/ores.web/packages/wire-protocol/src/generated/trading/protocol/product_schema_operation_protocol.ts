/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { ScriptInput } from '../domain/script_input.js';
import type { ScriptInputCode } from '../domain/script_input_code.js';

/**
 * @brief One field of a schema type a product reaches.
 */
export interface ProductField {
    /**
     * @brief The schema type the field belongs to, or trade for the data element of a trade type.
     */
    type_name: string;
    /**
     * @brief The field's element or attribute name, as ORE writes it.
     */
    name: string;
    /**
     * @brief The field's place among the fields of its type.
     */
    position: number;
    /**
     * @brief The field's label.
     */
    label: string;
    /**
     * @brief One sentence of help.
     */
    help: string;
    /**
     * @brief What the field means, from the ORE guide where it states it.
     */
    description: string;
    /**
     * @brief The kind of value: string, boolean, integer, double, date, record:TYPE or list:TYPE.
     */
    value_domain: string;
    /**
     * @brief Whether a trade must give the field.
     */
    is_required: boolean;
    /**
     * @brief The default, or a sentence saying there is none.
     */
    default_text: string;
    /**
     * @brief How a value is written, with the values an enumeration allows.
     */
    format: string;
    /**
     * @brief The field's path relative to its type.
     */
    xpath: string;
}

/**
 * @brief One form a product may be booked in.
 */
export interface ProductForm {
    /**
     * @brief native, for the product's own element, or scripted, for a ScriptedTrade over a library script.
     */
    kind: string;
    /**
     * @brief The library script a scripted form runs; empty for the native form.
     */
    script_name: string;
    /**
     * @brief The product tag of that script, which a pricing configuration assigns an engine to; empty for the native form.
     */
    product_tag: string;
}

/**
 * @brief One product that can be described.
 */
export interface ProductSummary {
    /**
     * @brief The product's code: a product type's code, or a library script's name for a script-only product.
     */
    code: string;
    /**
     * @brief The product's name.
     */
    name: string;
    /**
     * @brief How ORE builds the product: native, scripted, script or container; script for a script-only product.
     */
    construction: string;
}

/**
 * @brief Describes one product: its fields and its forms.
 */
export interface DescribeProductRequest {
    /**
     * @brief The product's code: a product type's code or a library script's name.
     */
    code: string;
}

/**
 * @brief The description of a product.
 */
export interface DescribeProductResponse {
    /**
     * @brief Whether the product was described.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief The product described.
     */
    product: ProductSummary;
    /**
     * @brief The element a trade of the product is read under; empty for a script-only product.
     */
    element: string;
    /**
     * @brief The fields of the trade's data element and of every type its fields reach, the element's own row first.
     */
    fields: ProductField[];
    /**
     * @brief The inputs of each script a form runs.
     */
    inputs: ScriptInput[];
    /**
     * @brief The codes of the coded inputs among them.
     */
    codes: ScriptInputCode[];
    /**
     * @brief The forms the product may be booked in.
     */
    forms: ProductForm[];
}

/**
 * @brief Lists the products that can be described.
 */
export interface ListProductsRequest {}

/**
 * @brief The products that can be described.
 */
export interface ListProductsResponse {
    /**
     * @brief Whether the list was read.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief Every product type, then every library script no product type twins, each by its code.
     */
    products: ProductSummary[];
}

export const subjects = {
    describe_product_request: 'trading.v1.ops.describe_product',
    list_products_request: 'trading.v1.ops.list_products',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    describe_product_request: true,
    list_products_request: true,
} as const;
