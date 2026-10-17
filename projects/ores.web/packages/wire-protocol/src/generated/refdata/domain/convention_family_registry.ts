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
 * Generator: projects/ores.codegen/scripts/regenerate_convention_family_registry.py
 * The TypeScript twin of the C++ convention family registry. To change an
 * entry, edit the convention entity model's #+convention_family: or
 * #+convention_write: declaration and regenerate.
 */
export type ConventionWriteMode = 'party' | 'by_id' | 'world';

/** One convention family, as the conventions screen lists it. */
export interface ConventionFamilyEntry {
    /** The family's key in the screen, and the import document's family. */
    readonly key: string;
    /** The entity model that serves the family. */
    readonly entity: string;
    /** The protocol subject prefix every verb of the entity shares. */
    readonly subjectStem: string;
    /** Whether the family's table carries a party_id column. */
    readonly partyScoped: boolean;
    /** How the import document writes the family's rows. */
    readonly writeMode: ConventionWriteMode;
}

/** A family drawn as one central convention row plus its parts. */
export interface CompositeConventionFamilyEntry {
    /** The family's key in the screen. */
    readonly key: string;
    /** The family model that serves the composite. */
    readonly entity: string;
    /** The family protocol's subject prefix. */
    readonly subjectStem: string;
    /** The kind_code every central row of the family carries. */
    readonly kindCode: string;
    /** The central entity the family's parts hang off. */
    readonly centralEntity: string;
    /** The part entity models the family carries. */
    readonly parts: readonly string[];
}

export const conventionFamilies: readonly ConventionFamilyEntry[] = [
    {
        key: 'average-ois',
        entity: 'average_ois_convention',
        subjectStem: 'refdata.v1.average_ois_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'bma-basis-swap',
        entity: 'bma_basis_swap_convention',
        subjectStem: 'refdata.v1.bma_basis_swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'bond-yield',
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
        key: 'cms-spread-option',
        entity: 'cms_spread_option_convention',
        subjectStem: 'refdata.v1.cms_spread_option_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'commodity-forward',
        entity: 'commodity_forward_convention',
        subjectStem: 'refdata.v1.commodity_forward_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'commodity-future',
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
        key: 'cross-currency-basis',
        entity: 'cross_currency_basis_convention',
        subjectStem: 'refdata.v1.cross_currency_basis_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'cross-currency-fix-float',
        entity: 'cross_currency_fix_float_convention',
        subjectStem: 'refdata.v1.cross_currency_fix_float_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'currency-pair',
        entity: 'currency_pair_convention',
        subjectStem: 'refdata.v1.currency_pair_conventions',
        partyScoped: false,
        writeMode: 'world',
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
        key: 'fx-option',
        entity: 'fx_option_convention',
        subjectStem: 'refdata.v1.fx_option_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'ibor-index',
        entity: 'ibor_index_convention',
        subjectStem: 'refdata.v1.ibor_index_conventions',
        partyScoped: false,
        writeMode: 'world',
    },
    {
        key: 'inflation-swap',
        entity: 'inflation_swap_convention',
        subjectStem: 'refdata.v1.inflation_swap_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'intraday-power-load',
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
        key: 'overnight-index',
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
        key: 'swap-index',
        entity: 'swap_index_convention',
        subjectStem: 'refdata.v1.swap_index_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
    {
        key: 'tenor',
        entity: 'tenor_convention',
        subjectStem: 'refdata.v1.tenor_conventions',
        partyScoped: false,
        writeMode: 'world',
    },
    {
        key: 'tenor-basis-swap-long-short',
        entity: 'tenor_basis_swap_long_short',
        subjectStem: 'refdata.v1.tenor_basis_swap_long_shorts',
        partyScoped: true,
        writeMode: 'by_id',
    },
    {
        key: 'tenor-basis-swap-pay-receive',
        entity: 'tenor_basis_swap_pay_receive',
        subjectStem: 'refdata.v1.tenor_basis_swap_pay_receives',
        partyScoped: true,
        writeMode: 'by_id',
    },
    {
        key: 'tenor-basis-two-swap',
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
        key: 'zero-inflation-index',
        entity: 'zero_inflation_index_convention',
        subjectStem: 'refdata.v1.zero_inflation_index_conventions',
        partyScoped: true,
        writeMode: 'party',
    },
];

export const compositeConventionFamilies: readonly CompositeConventionFamilyEntry[] = [
    {
        key: 'tenor-basis-swap',
        entity: 'tenor_basis_swap_convention',
        subjectStem: 'refdata.v1.tenor_basis_swap_conventions',
        kindCode: 'TENOR_BASIS_SWAP',
        centralEntity: 'convention',
        parts: ['tenor_basis_swap_pay_receive', 'tenor_basis_swap_long_short'],
    },
];
