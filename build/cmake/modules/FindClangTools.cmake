#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Tries to find clang-tidy and clang-format.
#
# Usage:
#
#  find_package(ClangTools)
#
# Optional variable:
#
#  ClangToolsBin_HOME  — when set, inspected before standard locations.
#
# Defines:
#
#  CLANG_TIDY_BIN    — path to clang-tidy binary
#  CLANG_TIDY_FOUND  — 1 if clang-tidy was found, 0 otherwise
#  CLANG_FORMAT_BIN  — path to clang-format binary
#  CLANG_FORMAT_FOUND — 1 if clang-format was found, 0 otherwise

find_program(CLANG_TIDY_BIN
    NAMES
        clang-tidy-20
        clang-tidy-19
        clang-tidy-18
        clang-tidy-17
        clang-tidy-16
        clang-tidy-15
        clang-tidy-14
        clang-tidy-13
        clang-tidy-12
        clang-tidy-11
        clang-tidy-10
        clang-tidy-9
        clang-tidy-8
        clang-tidy-7
        clang-tidy-6.0
        clang-tidy-5.0
        clang-tidy
    PATHS ${ClangTools_PATH} $ENV{CLANG_TOOLS_PATH} /usr/local/bin /usr/bin
    NO_DEFAULT_PATH
)

if ("${CLANG_TIDY_BIN}" STREQUAL "CLANG_TIDY_BIN-NOTFOUND")
    set(CLANG_TIDY_FOUND 0)
else()
    set(CLANG_TIDY_FOUND 1)
endif()

find_program(CLANG_FORMAT_BIN
    NAMES
        clang-format-20
        clang-format-19
        clang-format-18
        clang-format-17
        clang-format-16
        clang-format-15
        clang-format-14
        clang-format-13
        clang-format-12
        clang-format-11
        clang-format-10
        clang-format-9
        clang-format-8
        clang-format-7
        clang-format-6.0
        clang-format-5.0
        clang-format
    PATHS ${ClangTools_PATH} $ENV{CLANG_TOOLS_PATH} /usr/local/bin /usr/bin
    NO_DEFAULT_PATH
)

if ("${CLANG_FORMAT_BIN}" STREQUAL "CLANG_FORMAT_BIN-NOTFOUND")
    set(CLANG_FORMAT_FOUND 0)
else()
    set(CLANG_FORMAT_FOUND 1)
endif()
