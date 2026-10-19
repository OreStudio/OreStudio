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
 * Template: ts_convention_family_registry.ts.mustache
 * To modify, update the template and regenerate.
 */
export type ConventionWriteMode = 'party' | 'by_id' | 'world';

/**
 * One convention family the conventions document carries.
 *
 * key is the family's field name in the document, entity is the entity model
 * that serves it, subjectStem is the protocol prefix every verb of that entity
 * shares, partyScoped is true when the family's table carries a party_id
 * column, and writeMode is how the document writes the family.
 */
export interface ConventionFamilyEntry {
    readonly key: string;
    readonly entity: string;
    readonly subjectStem: string;
    readonly partyScoped: boolean;
    readonly writeMode: ConventionWriteMode;
}

/** Every convention family, ordered by family key. */
export const CONVENTION_FAMILY_ENTRIES: readonly ConventionFamilyEntry[] = [
    {
        key: 'average_ois',
        entity: 'average_ois_convention',
        subjectStem: 'refdata.v1.average_ois_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'bma_basis_swap',
        entity: 'bma_basis_swap_convention',
        subjectStem: 'refdata.v1.bma_basis_swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'bond_yield',
        entity: 'bond_yield_convention',
        subjectStem: 'refdata.v1.bond_yield_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'cds',
        entity: 'cds_convention',
        subjectStem: 'refdata.v1.cds_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'cms_spread_option',
        entity: 'cms_spread_option_convention',
        subjectStem: 'refdata.v1.cms_spread_option_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'commodity_forward',
        entity: 'commodity_forward_convention',
        subjectStem: 'refdata.v1.commodity_forward_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'commodity_future',
        entity: 'commodity_future_convention',
        subjectStem: 'refdata.v1.commodity_future_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'convention',
        entity: 'convention',
        subjectStem: 'refdata.v1.conventions',
        partyScoped: true,
        writeMode: 'by_id',
    },
    {
        key: 'cross_currency_basis',
        entity: 'cross_currency_basis_convention',
        subjectStem: 'refdata.v1.cross_currency_basis_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'cross_currency_fix_float',
        entity: 'cross_currency_fix_float_convention',
        subjectStem: 'refdata.v1.cross_currency_fix_float_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'deposit',
        entity: 'deposit_convention',
        subjectStem: 'refdata.v1.deposit_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'fra',
        entity: 'fra_convention',
        subjectStem: 'refdata.v1.fra_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'future',
        entity: 'future_convention',
        subjectStem: 'refdata.v1.future_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'fx_option',
        entity: 'fx_option_convention',
        subjectStem: 'refdata.v1.fx_option_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'ibor_index',
        entity: 'ibor_index_convention',
        subjectStem: 'refdata.v1.ibor_index_conventions',
        partyScoped: false,
        writeMode: 'world',
    },
    {
        key: 'inflation_swap',
        entity: 'inflation_swap_convention',
        subjectStem: 'refdata.v1.inflation_swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'intraday_power_load',
        entity: 'intraday_power_load_convention',
        subjectStem: 'refdata.v1.intraday_power_load_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'ois',
        entity: 'ois_convention',
        subjectStem: 'refdata.v1.ois_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'overnight_index',
        entity: 'overnight_index_convention',
        subjectStem: 'refdata.v1.overnight_index_conventions',
        partyScoped: false,
        writeMode: 'world',
    },
    {
        key: 'swap',
        entity: 'swap_convention',
        subjectStem: 'refdata.v1.swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'swap_index',
        entity: 'swap_index_convention',
        subjectStem: 'refdata.v1.swap_index_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'tenor_basis_swap_long_short',
        entity: 'tenor_basis_swap_long_short',
        subjectStem: 'refdata.v1.tenor_basis_swap_long_shorts',
        partyScoped: true,
        writeMode: 'by_id',
    },
    {
        key: 'tenor_basis_swap_pay_receive',
        entity: 'tenor_basis_swap_pay_receive',
        subjectStem: 'refdata.v1.tenor_basis_swap_pay_receives',
        partyScoped: true,
        writeMode: 'by_id',
    },
    {
        key: 'tenor_basis_two_swap',
        entity: 'tenor_basis_two_swap_convention',
        subjectStem: 'refdata.v1.tenor_basis_two_swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'zero',
        entity: 'zero_convention',
        subjectStem: 'refdata.v1.zero_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'zero_inflation_index',
        entity: 'zero_inflation_index_convention',
        subjectStem: 'refdata.v1.zero_inflation_index_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
];
