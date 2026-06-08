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
#ifndef ORES_QT_OVERNIGHT_INDEX_CONVENTION_HISTORY_DIALOG_HPP
#define ORES_QT_OVERNIGHT_INDEX_CONVENTION_HISTORY_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include <QString>
#include <memory>
#include <vector>

namespace Ui {
class OvernightIndexConventionHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing the version history of a overnight index convention.
 *
 * Shows all historical versions of a overnight index convention with ability
 * to view details or revert to a previous version.
 */
class OvernightIndexConventionHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.overnight_index_convention_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OvernightIndexConventionHistoryDialog(const QString& code,
                                                   ClientManager* clientManager,
                                                   QWidget* parent = nullptr);
    ~OvernightIndexConventionHistoryDialog() override;

    void loadHistory() override;

    /**
     * @brief Returns the identifier of the overnight index convention.
     */
    [[nodiscard]] QString code() const override {
        return code_;
    }

signals:
    void openVersionRequested(const refdata::domain::overnight_index_convention& ni,
                              int versionNumber);
    void revertVersionRequested(const refdata::domain::overnight_index_convention& ni);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int current_index, int previous_index) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    std::unique_ptr<Ui::OvernightIndexConventionHistoryDialog> ui_;
    QString code_;
    ClientManager* clientManager_;
    std::vector<refdata::domain::overnight_index_convention> versions_;
};

}

#endif
