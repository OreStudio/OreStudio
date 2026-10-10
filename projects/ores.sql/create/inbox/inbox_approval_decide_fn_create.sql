/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Decides an approval request in one statement.
 *
 * Writing the decision and moving the request to the state its decisions
 * reach is one step: a decision without its state, or a state without its
 * decision, would leave a request whose history contradicts it. So both are
 * written here, inside the caller's statement, with the request row locked.
 *
 * The rules, in order:
 *  - the request exists and the caller saw its current version;
 *  - it is open: waiting or held;
 *  - an open request past expires_at is closed as expired, not decided;
 *  - the decision is one the state allows: from waiting, approve, refuse,
 *    hold or withdraw; from held, approve, refuse or resume;
 *  - hold only for a kind that allows holding;
 *  - a comment when the decision type, or the kind for an approval, needs one.
 *
 * The insert trigger on the decisions table adds four-eyes, and the unique
 * index one approval per person. An approval closes the request only once the
 * kind's approvals_required approvals stand.
 *
 * A request that names parts is decided by part instead of by count:
 *  - an approval or a refusal names one of the request's parts;
 *  - a part with an approval already standing cannot answer again;
 *  - a part cannot answer until every part of an earlier answer_order has
 *    approved, so equal orders answer in parallel;
 *  - the request is approved when every part has approved, and one refusal
 *    from any part refuses it.
 * The caller checks that the person holds the part's decider permission. A
 * request with no part rows is decided by the count above.
 *
 * The function runs as the caller, so row-level security scopes every read to
 * the caller's tenant. It answers one row: the outcome (ok, missing, conflict,
 * invalid), a message, and the request's state and version afterwards.
 */
drop function if exists ores_inbox_decide_approval_request_fn(uuid, integer, text, uuid, text, text);

create or replace function ores_inbox_decide_approval_request_fn(
    p_request_id uuid,
    p_version integer,
    p_decision_code text,
    p_decided_by uuid,
    p_comment text,
    p_actor text,
    p_part_code text default null
) returns table (outcome text, message text, state_code text, version integer)
as $$
declare
    v_request ores_inbox_approval_requests_tbl%rowtype;
    v_kind ores_inbox_approval_kinds_tbl%rowtype;
    v_type ores_inbox_approval_decision_types_tbl%rowtype;
    v_next_state text;
    v_approvals integer;
    v_has_parts boolean;
    v_part_order integer;
    v_open_parts integer;
begin
    select * into v_request
    from ores_inbox_approval_requests_tbl r
    where r.id = p_request_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return query select 'missing'::text, 'No such request.'::text, null::text, null::integer;
        return;
    end if;

    if p_version <> v_request.version then
        return query select 'conflict'::text,
            'The request changed since it was read; read it again.'::text,
            v_request.state_code, v_request.version;
        return;
    end if;

    if v_request.state_code not in ('waiting', 'held') then
        return query select 'conflict'::text,
            format('The request is %s and can no longer be decided.', v_request.state_code),
            v_request.state_code, v_request.version;
        return;
    end if;

    if v_request.expires_at is not null and v_request.expires_at < clock_timestamp() then
        v_request.state_code := 'expired';
        v_request.modified_by := p_actor;
        v_request.change_reason_code := 'system.update';
        v_request.change_commentary := 'Expired before it was decided';
        insert into ores_inbox_approval_requests_tbl select v_request.*;
        return query select 'conflict'::text, 'The request expired before it was decided.'::text,
            'expired'::text, v_request.version + 1;
        return;
    end if;

    select * into v_type
    from ores_inbox_approval_decision_types_tbl t
    where t.tenant_id = ores_utility_system_tenant_id_fn()
      and t.code = p_decision_code
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return query select 'invalid'::text, format('Unknown decision %s.', p_decision_code),
            v_request.state_code, v_request.version;
        return;
    end if;

    select * into v_kind
    from ores_inbox_approval_kinds_tbl k
    where k.tenant_id = ores_utility_system_tenant_id_fn()
      and k.code = v_request.kind_code
      and k.valid_to = ores_utility_infinity_timestamp_fn();

    if not (
        (v_request.state_code = 'waiting'
            and p_decision_code in ('approve', 'refuse', 'hold', 'withdraw'))
        or (v_request.state_code = 'held'
            and p_decision_code in ('approve', 'refuse', 'resume'))
    ) then
        return query select 'invalid'::text,
            format('A %s request cannot take the decision %s.', v_request.state_code, p_decision_code),
            v_request.state_code, v_request.version;
        return;
    end if;

    if p_decision_code = 'hold' and not v_kind.allows_hold then
        return query select 'invalid'::text, 'This kind of request cannot be held.'::text,
            v_request.state_code, v_request.version;
        return;
    end if;

    if coalesce(btrim(p_comment), '') = ''
       and (v_type.requires_comment or (p_decision_code = 'approve' and v_kind.comment_on_approve)) then
        return query select 'invalid'::text, format('A %s needs a comment.', v_type.name),
            v_request.state_code, v_request.version;
        return;
    end if;

    select exists (
        select 1 from ores_inbox_approval_request_parts_tbl rp
        where rp.request_id = p_request_id
          and rp.valid_to = ores_utility_infinity_timestamp_fn()
    ) into v_has_parts;

    if v_has_parts and p_decision_code in ('approve', 'refuse') then
        select p.answer_order into v_part_order
        from ores_inbox_approval_request_parts_tbl rp
        join ores_inbox_approval_parts_tbl p
          on p.tenant_id = ores_utility_system_tenant_id_fn()
         and p.code = rp.part_code
         and p.valid_to = ores_utility_infinity_timestamp_fn()
        where rp.request_id = p_request_id
          and rp.part_code = p_part_code
          and rp.valid_to = ores_utility_infinity_timestamp_fn();

        if not found then
            return query select 'invalid'::text,
                'Name one of the parts this request needs.'::text,
                v_request.state_code, v_request.version;
            return;
        end if;

        if exists (
            select 1 from ores_inbox_approval_decisions_tbl d
            where d.request_id = p_request_id
              and d.part_code = p_part_code
              and d.decision_code = 'approve'
              and d.valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            return query select 'conflict'::text,
                format('The %s part has already approved.', p_part_code),
                v_request.state_code, v_request.version;
            return;
        end if;

        if exists (
            select 1
            from ores_inbox_approval_request_parts_tbl rp
            join ores_inbox_approval_parts_tbl p
              on p.tenant_id = ores_utility_system_tenant_id_fn()
             and p.code = rp.part_code
             and p.valid_to = ores_utility_infinity_timestamp_fn()
            where rp.request_id = p_request_id
              and rp.valid_to = ores_utility_infinity_timestamp_fn()
              and p.answer_order < v_part_order
              and not exists (
                  select 1 from ores_inbox_approval_decisions_tbl d
                  where d.request_id = p_request_id
                    and d.part_code = rp.part_code
                    and d.decision_code = 'approve'
                    and d.valid_to = ores_utility_infinity_timestamp_fn())
        ) then
            return query select 'conflict'::text,
                'An earlier part must approve before this part can answer.'::text,
                v_request.state_code, v_request.version;
            return;
        end if;
    end if;

    insert into ores_inbox_approval_decisions_tbl (
        id, tenant_id, version, request_id, decision_code, part_code, decided_by, decided_at, comment,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (
        gen_random_uuid(), v_request.tenant_id, 0, p_request_id, p_decision_code,
        case when v_has_parts and p_decision_code in ('approve', 'refuse')
             then p_part_code end,
        p_decided_by, clock_timestamp(), coalesce(p_comment, ''),
        p_actor, p_actor, 'system.new_record', '');

    if p_decision_code = 'approve' and v_has_parts then
        select count(*) into v_open_parts
        from ores_inbox_approval_request_parts_tbl rp
        where rp.request_id = p_request_id
          and rp.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (
              select 1 from ores_inbox_approval_decisions_tbl d
              where d.request_id = p_request_id
                and d.part_code = rp.part_code
                and d.decision_code = 'approve'
                and d.valid_to = ores_utility_infinity_timestamp_fn());
        v_next_state := case when v_open_parts = 0
                             then 'approved' else v_request.state_code end;
    elsif p_decision_code = 'approve' then
        select count(*) into v_approvals
        from ores_inbox_approval_decisions_tbl d
        where d.request_id = p_request_id
          and d.decision_code = 'approve'
          and d.valid_to = ores_utility_infinity_timestamp_fn();
        v_next_state := case when v_approvals >= v_kind.approvals_required
                             then 'approved' else v_request.state_code end;
    else
        v_next_state := case p_decision_code
            when 'refuse' then 'refused'
            when 'hold' then 'held'
            when 'resume' then 'waiting'
            when 'withdraw' then 'withdrawn'
        end;
    end if;

    if v_next_state <> v_request.state_code then
        v_request.state_code := v_next_state;
        v_request.modified_by := p_actor;
        v_request.change_reason_code := 'system.update';
        v_request.change_commentary := format('Decision: %s', p_decision_code);
        insert into ores_inbox_approval_requests_tbl select v_request.*;
        return query select 'ok'::text, ''::text, v_next_state, v_request.version + 1;
        return;
    end if;

    return query select 'ok'::text, ''::text, v_request.state_code, v_request.version;
end;
$$ language plpgsql;
