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
#ifndef ORES_QT_CHANGE_REASON_DIALOG_HPP
#define ORES_QT_CHANGE_REASON_DIALOG_HPP

#include <QDialog>
#include <QComboBox>
#include <QTextEdit>
#include <QLabel>
#include <QPushButton>
#include <QDialogButtonBox>
#include <vector>
#include "ores.iam/domain/change_reason.hpp"

namespace ores::qt {

/**
 * @brief Dialog for selecting a change reason when saving/deleting entities.
 *
 * This dialog is displayed when a user saves changes to an entity that
 * requires change tracking. It presents a list of applicable reasons
 * and optionally requires commentary based on the selected reason.
 *
 * Usage:
 * @code
 * ChangeReasonDialog dialog(reasons, ChangeReasonDialog::OperationType::Amend, this);
 * if (dialog.exec() == QDialog::Accepted) {
 *     std::string reason_code = dialog.selectedReasonCode();
 *     std::string commentary = dialog.commentary();
 * }
 * @endcode
 */
class ChangeReasonDialog final : public QDialog {
    Q_OBJECT

public:
    /**
     * @brief Type of operation requiring a change reason.
     */
    enum class OperationType {
        Amend,   ///< Updating an existing entity
        Delete   ///< Deleting an entity
    };

    /**
     * @brief Construct the dialog.
     *
     * @param reasons List of applicable change reasons
     * @param operation Type of operation (Amend or Delete)
     * @param hasFieldChanges True if any entity fields have been modified. When false,
     *        this is a "touch" operation and only "common.non_material_update" is valid.
     *        When true, "common.non_material_update" is disabled.
     * @param parent Parent widget
     */
    explicit ChangeReasonDialog(
        const std::vector<iam::domain::change_reason>& reasons,
        OperationType operation,
        bool hasFieldChanges,
        QWidget* parent = nullptr);

    ~ChangeReasonDialog() override = default;

    /**
     * @brief Get the selected reason code.
     *
     * @return The code of the selected reason (e.g., "common.rectification")
     */
    std::string selectedReasonCode() const;

    /**
     * @brief Get the commentary entered by the user.
     *
     * @return Commentary text (may be empty if not required)
     */
    std::string commentary() const;

private slots:
    void onReasonChanged(int index);
    void onCommentaryChanged();
    void validateAndAccept();

private:
    void setupUi();
    void updateValidation();

    std::vector<iam::domain::change_reason> reasons_;
    OperationType operation_;
    bool hasFieldChanges_;

    QComboBox* reasonCombo_;
    QLabel* descriptionLabel_;
    QLabel* commentaryLabel_;
    QTextEdit* commentaryEdit_;
    QLabel* requiredLabel_;
    QDialogButtonBox* buttonBox_;
    QPushButton* saveButton_;

    bool commentary_required_{false};
};

}

#endif
