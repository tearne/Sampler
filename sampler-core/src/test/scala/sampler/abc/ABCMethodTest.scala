/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.abc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar

class ABCMethodTest extends AssertionsForJUnit with MockitoSugar{
	@Test def initialisesAndIterates {
		
		val instance = new ABCMethod{
			val initialise = mock[Initialise]
			val iterate = mock[Iterate]
			val statistics = mock[Statistics]
		}
		
		
		fail("todo")
		
	}
}