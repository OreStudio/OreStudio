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
#ifndef ORES_QT_JOB_INSTANCE_DETAIL_DIALOG_HPP
#define ORES_QT_JOB_INSTANCE_DETAIL_DIALOG_HPP

#include <QDialog>
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"

namespace ores::qt {

/**
 * @brief Read-only detail dialog for a single job instance execution record.
 */
class JobInstanceDetailDialog final : public QDialog {
    Q_OBJECT

public:
    explicit JobInstanceDetailDialog(
        const scheduler::messaging::job_instance_summary& instance,
        QWidget* parent = nullptr);
    ~JobInstanceDetailDialog() override = default;
};

}

#endif
