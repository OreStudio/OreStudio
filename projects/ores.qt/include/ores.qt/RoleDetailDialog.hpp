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
#ifndef ORES_QT_ROLE_DETAIL_DIALOG_HPP
#define ORES_QT_ROLE_DETAIL_DIALOG_HPP

#include <memory>
#include "ores.iam/domain/role.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"

namespace Ui {

class RoleDetailDialog;

}

namespace ores::qt {

/**
 * @brief Dialog widget for viewing role details.
 *
 * This dialog displays role information in read-only mode including:
 * - Role name and description
 * - List of assigned permissions
 * - Metadata (version, modified by, modified at)
 */
class RoleDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.role_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit RoleDetailDialog(QWidget* parent = nullptr);
    ~RoleDetailDialog() override;

    /**
     * @brief Sets the role to display.
     *
     * @param role The role to display
     */
    void setRole(const iam::domain::role& role);

    /**
     * @brief Gets the current role.
     *
     * @return The role being displayed
     */
    [[nodiscard]] const iam::domain::role& getRole() const { return currentRole_; }

    /**
     * @brief Clears all form fields.
     */
    void clearDialog();

signals:

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;

private:
    void populatePermissionsList();

private:
    std::unique_ptr<Ui::RoleDetailDialog> ui_;
    iam::domain::role currentRole_;
};

}

#endif
