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
#ifndef ORES_QT_METHODOLOGY_HISTORY_DIALOG_HPP
#define ORES_QT_METHODOLOGY_HISTORY_DIALOG_HPP

#include "ores.dq.api/domain/methodology.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include <QString>
#include <memory>
#include <string>
#include <vector>

namespace Ui {
class MethodologyHistoryDialog;
}

namespace ores::qt {

class MethodologyHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.methodology_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MethodologyHistoryDialog(const std::string& name,
                                      ClientManager* clientManager,
                                      QWidget* parent = nullptr);
    ~MethodologyHistoryDialog() override;

    void loadHistory() override;

    /**
     * @brief Returns the identifier of the methodology.
     */
    [[nodiscard]] QString code() const override {
        return QString::fromStdString(name_);
    }

signals:
    void openVersionRequested(const dq::domain::methodology& methodology, int versionNumber);
    void revertVersionRequested(const dq::domain::methodology& methodology);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int current_index, int previous_index) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    std::unique_ptr<Ui::MethodologyHistoryDialog> ui_;
    std::string name_;
    ClientManager* clientManager_;
    std::vector<dq::domain::methodology> versions_;
};

}

#endif
