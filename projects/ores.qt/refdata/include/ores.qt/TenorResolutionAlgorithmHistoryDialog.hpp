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
#ifndef ORES_QT_TENOR_RESOLUTION_ALGORITHM_HISTORY_DIALOG_HPP
#define ORES_QT_TENOR_RESOLUTION_ALGORITHM_HISTORY_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include "ores.refdata.api/domain/tenor_resolution_algorithm.hpp"

namespace Ui {
class TenorResolutionAlgorithmHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing the version history of a tenor resolution algorithm.
 *
 * Shows all historical versions of a tenor resolution algorithm with ability
 * to view details or revert to a previous version.
 */
class TenorResolutionAlgorithmHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenor_resolution_algorithm_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TenorResolutionAlgorithmHistoryDialog(const QString& code,
                                                   ClientManager* clientManager,
                                                   QWidget* parent = nullptr);
    ~TenorResolutionAlgorithmHistoryDialog() override;

    void loadHistory() override;
    [[nodiscard]] QString code() const override;

signals:
    void openVersionRequested(const refdata::domain::tenor_resolution_algorithm& algorithm,
                              int versionNumber);
    void revertVersionRequested(const refdata::domain::tenor_resolution_algorithm& algorithm);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int ci, int pi) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    Ui::TenorResolutionAlgorithmHistoryDialog* ui_;
    QString code_;
    ClientManager* clientManager_;
    std::vector<refdata::domain::tenor_resolution_algorithm> versions_;
};

}

#endif
