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
import type { DatasetBundleMember } from '../domain/dataset_bundle_member.js';

export interface GetDatasetBundleMembersRequest {
    offset: number;
    limit: number;
}

export interface GetDatasetBundleMembersResponse {
    dataset_bundle_members: DatasetBundleMember[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetDatasetBundleMembersByBundleRequest {
    bundle_code: string;
    offset: number;
    limit: number;
}

export interface GetDatasetBundleMembersByBundleResponse {
    dataset_bundle_members: DatasetBundleMemberView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDatasetBundleMemberRequest {
    dataset_bundle_members: DatasetBundleMember[];
}

export interface SaveDatasetBundleMemberResponse {
    success: boolean;
    message: string;
}

export interface DeleteDatasetBundleMemberRequest {
    bundle_codes: string[];
    dataset_codes: string[];
}

export interface DeleteDatasetBundleMemberResponse {
    success: boolean;
    message: string;
}

export interface CountDatasetBundleMembersByBundleRequest {
    bundle_code: string;
}

export interface CountDatasetBundleMembersByBundleResponse {
    total_available_count: number;
}

export interface CountDatasetBundleMembersByDatasetRequest {
    dataset_code: string;
}

export interface CountDatasetBundleMembersByDatasetResponse {
    total_available_count: number;
}

export interface DatasetBundleMemberView {
    dataset_bundle_member: DatasetBundleMember;
}

export const subjects = {
    get_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.list",
    get_dataset_bundle_members_by_bundle_request: "dq.v1.dataset_bundle_members.list_by_bundle_code",
    save_dataset_bundle_member_request: "dq.v1.dataset_bundle_members.save",
    delete_dataset_bundle_member_request: "dq.v1.dataset_bundle_members.delete",
    count_dataset_bundle_members_by_bundle_request: "dq.v1.dataset_bundle_members.count_by_bundle_code",
    count_dataset_bundle_members_by_dataset_request: "dq.v1.dataset_bundle_members.count_by_dataset_code",
} as const;
