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
#ifndef ORES_QT_TENANT_DETAIL_DIALOG_HPP
#define ORES_QT_TENANT_DETAIL_DIALOG_HPP

#include <QToolBar>
#include <QAction>
#include <memory>
#include "ores.iam/domain/tenant.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.logging/make_logger.hpp"

namespace Ui {

class TenantDetailDialog;

}

namespace ores::qt {

/**
 * @brief Dialog widget for creating and editing tenants.
 *
 * Tenants are the core multi-tenancy entities representing isolated
 * organisations. This dialog provides:
 *
 * In create mode (new tenant):
 * - Code, name, type, hostname, status, and description are editable
 * - Metadata section is hidden
 *
 * In edit mode (existing tenant):
 * - Code is read-only (primary key)
 * - All other fields can be modified
 * - Metadata shows version, recorded by, recorded at
 *
 * In read-only mode (historical version):
 * - All fields are disabled
 * - Revert button is shown instead of save/delete
 */
class TenantDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TenantDetailDialog(QWidget* parent = nullptr);
    ~TenantDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);

    /**
     * @brief Sets the tenant to display/edit.
     *
     * If tenant has an empty code, dialog enters create mode.
     * Otherwise, dialog enters edit mode with code read-only.
     *
     * @param tenant The tenant to display/edit
     */
    void setTenant(const iam::domain::tenant& tenant);

    /**
     * @brief Gets the current tenant state from the form.
     *
     * @return The tenant with values from the form fields
     */
    [[nodiscard]] iam::domain::tenant getTenant() const;

    /**
     * @brief Clears all form fields and resets dialog state.
     */
    void clearDialog();

    /**
     * @brief Saves the tenant (create or update).
     */
    void save();

    /**
     * @brief Sets the dialog to read-only mode for viewing historical versions.
     *
     * In read-only mode:
     * - All fields are disabled
     * - Save button is hidden
     * - Delete button is hidden
     * - Revert button is shown
     *
     * @param readOnly True to enable read-only mode
     * @param versionNumber The historical version number being displayed
     */
    void setReadOnly(bool readOnly, int versionNumber = 0);

    /**
     * @brief Mark the dialog data as stale.
     *
     * Called when a notification is received indicating this tenant has
     * changed on the server.
     */
    void markAsStale();

    /**
     * @brief Returns the code of the tenant being edited.
     */
    [[nodiscard]] QString tenantCode() const;

signals:
    void tenantSaved(const QString& code);
    void tenantDeleted(const QString& code);
    void isDirtyChanged(bool isDirty);

    /**
     * @brief Emitted when user requests to revert to the displayed historical version.
     * @param tenant The tenant data to revert to.
     */
    void revertRequested(const iam::domain::tenant& tenant);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onRevertClicked();
    void onFieldChanged();

private:
    void setupToolbar();
    void setCreateMode(bool createMode);
    void setFieldsReadOnly(bool readOnly);
    void updateSaveButtonState();
    void populateTypeCombo();
    void populateStatusCombo();

private:
    std::unique_ptr<Ui::TenantDetailDialog> ui_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    bool isStale_;
    int historicalVersion_;
    std::string modifiedByUsername_;
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* deleteAction_;
    QAction* revertAction_;

    ClientManager* clientManager_;
    iam::domain::tenant currentTenant_;
};

}

#endif
